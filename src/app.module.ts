import { Module } from '@nestjs/common';
import { ScheduleModule as CronModule } from '@nestjs/schedule';
import { PrismaService } from './prisma/prisma.service';
import { ApiCallerModule } from './modules/api-caller/api-caller.module';
import { ThrottlerModule } from '@nestjs/throttler';
import { WebhookModule } from './modules/webhook/webhook.module';
import { ConfigModule } from '@nestjs/config';
import { CommonModule } from './modules/common/common.module';
import { AuthModule } from './modules/auth/auth.module';
import { UserModule } from './modules/user/user.module';
import { GraphQlcallerModule } from './modules/graph-qlcaller/graph-qlcaller.module';
import { CollectionModule } from './modules/collection/collection.module';
import { NftModule } from './modules/nft/nft.module';
import { ValidatorModule } from './modules/validator/validator.module';
import { LaunchpadModule } from './modules/launchpad/launchpad.module';
import { HealthcheckController } from './modules/healthcheck/healthcheck.controller';
import { APP_INTERCEPTOR } from '@nestjs/core';
import { BigIntInterceptor } from './commons/interceptors/bigint.interceptor';
import { CMSModule } from './modules/cms/cms.module';
import { PlatformOverviewModule } from './modules/platform-overview/platform-overview.module';
import { PlatformTemplateModule } from './modules/platform-template/platform-template.module';
import { OrderModule } from './modules/order/order.module';
@Module({
  imports: [
    ConfigModule.forRoot({
      ignoreEnvFile: true,
    }),
    ThrottlerModule.forRoot({
      ttl: 60,
      limit: 80,
    }),
    CronModule.forRoot(),
    ApiCallerModule,
    WebhookModule,
    CommonModule,
    AuthModule,
    UserModule,
    GraphQlcallerModule,
    CollectionModule,
    NftModule,
    ValidatorModule,
    LaunchpadModule,
    CMSModule,
    PlatformOverviewModule,
    PlatformTemplateModule,
    OrderModule,
  ],
  providers: [
    PrismaService,
    {
      provide: APP_INTERCEPTOR,
      useClass: BigIntInterceptor,
    },
  ],
  controllers: [HealthcheckController],
})
export class AppModule {
  constructor() {}
}
