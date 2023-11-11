import { NestFactory } from '@nestjs/core';
import { AppModule } from './app.module';
import { Redis } from './database';
import * as compression from 'compression';
import * as session from 'express-session';
import * as cookieParser from 'cookie-parser';
// import helmet from 'helmet';
import { ValidationPipe } from '@nestjs/common';
// import { GraphQLErrorFilter } from './commons/errors/ExceptionFilter';
function matchRegexArray(arr: string[], str: string): boolean {
  for (const pattern of arr) {
    const regex = new RegExp(pattern);
    if (regex.test(str)) {
      return true;
    }
  }
  return false;
}

const whitelist = ['http://localhost:3000'];

async function bootstrap() {
  const app = await NestFactory.create(AppModule);

  app.use(cookieParser());
  app.use(
    session({
      secret: 'my-secret', // You should use a strong random secret
      resave: false,
      saveUninitialized: false,
      cookie: {
        httpOnly: true,
        secure: process.env.NODE_ENV === 'production',
      },
    }),
  );

  app.enableCors({
    origin: function (origin, callback) {
      if (!origin || matchRegexArray(whitelist, origin)) {
        callback(null, true);
      } else {
        callback(new Error('Not allowed by CORS'));
      }
    },
    credentials: true,
  });
  app.use(compression());
  // const redisConnectFn = Redis.getClient();
  // await redisConnectFn;
  await app.listen(8888);
}
bootstrap();
