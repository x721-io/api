import {
  CallHandler,
  ExecutionContext,
  Injectable,
  NestInterceptor,
} from '@nestjs/common';
import { Observable } from 'rxjs';
import { map } from 'rxjs/operators';

@Injectable()
export class BigIntInterceptor implements NestInterceptor {
  intercept(context: ExecutionContext, next: CallHandler): Observable<any> {
    return next
      .handle()
      .pipe(map((data) => this.transformBigIntToString(data)));
  }

  private transformBigIntToString(data: any): any {
    if (Array.isArray(data)) {
      return data.map((item) => this.transformBigIntToString(item));
    } else if (data !== null && typeof data === 'object') {
      Object.keys(data).forEach((key) => {
        if (typeof data[key] === 'bigint') {
          data[key] = data[key].toString();
        } else if (typeof data[key] === 'object') {
          data[key] = this.transformBigIntToString(data[key]);
        }
      });
      return data;
    }
    return data;
  }
}
