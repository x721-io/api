import { Module } from '@nestjs/common';
import { LaunchpadService } from './launchpad.service';
import { LaunchpadController } from './launchpad.controller';
import { PrismaService } from 'src/prisma/prisma.service';

@Module({
  controllers: [LaunchpadController],
  providers: [LaunchpadService, PrismaService],
})
export class LaunchpadModule {}
