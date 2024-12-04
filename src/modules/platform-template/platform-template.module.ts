import { Module } from '@nestjs/common';
import { PlatformTemplateService } from './platform-template.service';
import { PlatformTemplateController } from './platform-template.controller';
import { PrismaService } from '../../prisma/prisma.service';

@Module({
  controllers: [PlatformTemplateController],
  providers: [PlatformTemplateService, PrismaService],
})
export class PlatformTemplateModule {}
