import {
  Body,
  Controller,
  HttpCode,
  Post,
  UseGuards,
  UsePipes,
  ValidationPipe,
} from '@nestjs/common';
import { WebhookService } from './webhook.service';
import { ApiKeyAuthGuard } from '../auth/guards/api-key-auth-guard';
import { Casso } from 'src/commons/types/CassoReceiveResponse';
import { CarBrandsModels } from 'src/commons/types/VAPModelSyncResponse';
import {
  WEBHOOK_MARKET_REBRANDER,
  WEBHOOK_VUCAR_REBRANDER,
} from 'src/commons/definitions/Api.definition';

@Controller('webhook')
export class WebhookController {
  constructor(private webhookService: WebhookService) {}

}
