import { Module } from '@nestjs/common';
import { ApiCallerService } from './api-caller.service';
import { HttpModule } from '@nestjs/axios';

@Module({
  imports: [HttpModule],
  providers: [ApiCallerService],
  exports: [ApiCallerService],
})
export class ApiCallerModule {}
