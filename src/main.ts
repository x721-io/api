import { NestFactory } from '@nestjs/core';
import { AppModule } from './app.module';
import { Redis } from './database';
import * as compression from 'compression';
import * as session from 'express-session';
import * as cookieParser from 'cookie-parser';
// import helmet from 'helmet';
import { ValidationPipe } from '@nestjs/common';
import { DocumentBuilder, SwaggerModule } from '@nestjs/swagger';
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

const whitelist = [
  'http://localhost:3000',
  'https://marketplace-dev.uniultra.xyz/',
  'https://marketplace-dev.uniultra.xyz',
  'https://marketplace.uniultra.xyz/',
  'https://marketplace.uniultra.xyz',
];

async function bootstrap() {
  const app = await NestFactory.create(AppModule);

  app.use(cookieParser());

  app.useGlobalPipes(
    new ValidationPipe({
      transform: true,
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
  const config = new DocumentBuilder()
    .setTitle('NFT marketplace')
    .setDescription('The API description')
    .setVersion('1.0')
    .addTag('NFT Marketplace')
    .build();
  const document = SwaggerModule.createDocument(app, config);
  SwaggerModule.setup('api', app, document);
  const redisConnectFn = Redis.getClient();
  await redisConnectFn;
  await app.listen(process.env.PORT || 8888);
}
bootstrap();
