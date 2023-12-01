import { Module } from '@nestjs/common';
import { CommonService } from './common.service';
import { CommonController } from './common.controller';
import { PrismaService } from 'src/prisma/prisma.service';

@Module({
  controllers: [CommonController],
  providers: [CommonService, PrismaService],
})
export class CommonModule {}
