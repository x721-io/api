import { HttpService } from '@nestjs/axios';
import { Injectable } from '@nestjs/common';
import {
  map,
  retryWhen,
  delay,
  take,
  catchError,
  mergeMap,
} from 'rxjs/operators';
import { logger } from 'src/commons';
import { Observable, ObservableInput, throwError, timer } from 'rxjs';
import { ApiResponse } from 'src/commons/types/ApiResponse.common';
import * as querystring from 'querystring';

@Injectable()
export class ApiCallerService {
  constructor(private httpService: HttpService) {}

  async makePostRequest<T, R>(
    url: string,
    data: T,
    headers?: any,
  ): Promise<ApiResponse<R>> {
    try {
      logger.info(`API POST REQUEST: ${url}: ${JSON.stringify(data)}`);

      let requestBody: string;

      if (
        headers &&
        headers['Content-Type'] === 'application/x-www-form-urlencoded'
      ) {
        requestBody = querystring.stringify(
          data as querystring.ParsedUrlQueryInput,
        );
      } else {
        requestBody = JSON.stringify(data);
        headers['Content-Type'] = 'application/json';
      }

      const response: ApiResponse<R> = await this.httpService
        .post(url, requestBody, { headers })
        .pipe(
          map((response) => response.data),
          catchError((error) => {
            if (error.code === 'ECONNREFUSED') {
              // Retry the request if the error is due to a connection failure
              logger.warn(`API POST REQUEST FAILED: ${url}. Retrying...`);
              return throwError(() => error);
            } else {
              logger.error(`API POST REQUEST FAILED: ${url}, ${error.code}`);
              return throwError(() => error); // Continue with the error for non-ECONNREFUSED errors
            }
          }),
          retryWhen((errors: Observable<any>): Observable<any> => {
            return errors.pipe(
              mergeMap((error) => {
                if (error.code === 'ECONNREFUSED') {
                  return timer(1000); // Retry for ECONNREFUSED errors
                } else {
                  return throwError(() => error); // Skip retry for other errors
                }
              }),
              take(3),
            );
          }),
        )
        .toPromise();
      logger.info(`API RESPONSE: ${JSON.stringify(response)}`);
      return response;
    } catch (error) {
      logger.info('ERROR: ' + error);
      throw error;
    }
  }

  async callGetApi(url: string, headers?: any): Promise<any> {
    try {
      logger.info(`API POST REQUEST: ${url}`);
      const response = await this.httpService
        .get(url, { headers })
        .pipe(
          map((response) => response.data),
          catchError((error) => {
            if (error.code === 'ECONNREFUSED') {
              logger.warn(`API GET REQUEST FAILED: ${url}. Retrying...`);
              return throwError(() => new Error(error.message));
            } else {
              logger.error(`API GET REQUEST FAILED: ${url}, ${error.code}`);
              return throwError(() => new Error(error.message));
            }
          }),
          retryWhen((errors: Observable<any>): Observable<any> => {
            return errors.pipe(
              mergeMap((error) => {
                if (error.code === 'ECONNREFUSED') {
                  return timer(1000); // Retry for ECONNREFUSED errors
                } else {
                  return throwError(() => error); // Skip retry for other errors
                }
              }),
              take(3),
            );
          }),
        )
        .toPromise();
      logger.info(`API RESPONSE: ${JSON.stringify(response)}`);
      return response;
    } catch (error) {
      throw error;
    }
  }
}
