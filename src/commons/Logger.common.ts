import * as winston from 'winston';
import 'winston-daily-rotate-file';
import * as dotenv from 'dotenv';
import { v4 as uuidv4 } from 'uuid';

dotenv.config({ path: __dirname + '/.env' });

class LoggerUtil {
  logger: winston.Logger;

  constructor() {
    this.init();
  }

  private init(): void {
    const logID = uuidv4();
    const { combine, timestamp, printf, metadata } = winston.format;
    const myFormat = printf(
      ({ level, message, timestamp }) =>
        `${timestamp} ${level.toUpperCase()}: ${message}`,
    );
    this.logger = winston.createLogger({
      level: 'info',
      format: combine(
        timestamp(),
        myFormat,
        metadata({ fillExcept: ['timestamp', 'message', 'level'] }),
        winston.format.json(),
      ),
      defaultMeta: {
        service: 'u2u-service',
      },
      transports:
        process.env.NODE_ENV === 'production'
          ? [
              new winston.transports.DailyRotateFile({
                datePattern: 'DD-MM-YYYY',
                filename: `${
                  process.env.LOG_PATH || '/logs'
                }/${logID}.error.log`,
                level: 'error',
              }),
              new winston.transports.DailyRotateFile({
                datePattern: 'DD-MM-YYYY',
                filename: `${
                  process.env.LOG_PATH || '/logs'
                }/${logID}.debug.log`,
                level: 'debug',
              }),
              new winston.transports.DailyRotateFile({
                datePattern: 'DD-MM-YYYY',
                filename: `${
                  process.env.LOG_PATH || '/logs'
                }/${logID}.info.log`,
                level: 'info',
              }),
            ]
          : [],
    });
    this.logger.add(
      new winston.transports.Console({
        format: combine(timestamp(), myFormat),
      }),
    );
    if (
      process.env.NODE_ENV === 'production' &&
      (process.env.LOG_TYPE || 'EC2') === 'EC2'
    ) {
      // eslint-disable-next-line no-console
      console.log(
        `U2U LOGS PATH: ${process.env.LOG_PATH || '/logs'}/${logID}`,
      );
    }
  }
}

const loggerUtil = new LoggerUtil();
const logger = loggerUtil.logger;
export default logger;
