import { Controller, Get, Header, HttpCode } from '@nestjs/common';

@Controller('/')
export class HealthcheckController {
  @Get('/')
  @HttpCode(200)
  @Header(
    'Cache-Control',
    'no-store, no-cache, must-revalidate, proxy-revalidate',
  )
  @Header('Pragma', 'no-cache')
  healthCheck() {
    // You can add additional checks here if needed
    return { status: 'ok' };
  }
}
