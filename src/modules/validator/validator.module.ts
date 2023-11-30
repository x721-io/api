import { Module } from '@nestjs/common';
import { ValidatorService } from './validator.service';
import { ValidatorController } from './validator.controller';
import { PrismaService } from 'src/prisma/prisma.service';
@Module({
  providers: [ValidatorService, PrismaService],
  controllers: [ValidatorController],
})
export class ValidatorModule {}
