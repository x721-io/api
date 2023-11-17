import { Module } from '@nestjs/common';
import { NftService } from './nft.service';
import { NftController } from './nft.controller';
import { PrismaService } from 'src/prisma/prisma.service';

@Module({
  providers: [NftService, PrismaService],
  controllers: [NftController]
})
export class NftModule {}
