import { Controller, Get, HttpCode } from '@nestjs/common';

@Controller('/')
export class HealthcheckController {
  @Get('/')
  @HttpCode(200)
  healthCheck() {
    // You can add additional checks here if needed
    return { status: 'ok' };
  }
}
