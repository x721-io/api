import { Module } from '@nestjs/common';
import { PlatformOverviewService } from './platform-overview.service';
import { PlatformOverviewController } from './platform-overview.controller';
import { PrismaService } from '../../prisma/prisma.service';

@Module({
  controllers: [PlatformOverviewController],
  providers: [PlatformOverviewService, PrismaService],
})
export class PlatformOverviewModule {}
