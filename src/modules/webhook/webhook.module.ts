import { Module } from '@nestjs/common';
import { WebhookController } from './webhook.controller';
import { WebhookService } from './webhook.service';
import { PrismaService } from 'src/prisma/prisma.service';
import { Redis } from 'src/database';
import { ApiCallerModule } from '../api-caller/api-caller.module';

@Module({
  controllers: [WebhookController],
  providers: [
    WebhookService,
    PrismaService,
  ],
  imports: [ApiCallerModule],
})
export class WebhookModule {}
