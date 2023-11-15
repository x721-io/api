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
  ],
  providers: [
    PrismaService,
  ],
})
export class AppModule {
  constructor() {
  }
}
