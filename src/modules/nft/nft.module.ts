import { Module } from '@nestjs/common';
import { NftService } from './nft.service';
import { NftController } from './nft.controller';
import { PrismaService } from 'src/prisma/prisma.service';
import { TokenService } from './token.service';
import { GraphQlcallerService } from '../graph-qlcaller/graph-qlcaller.service';

@Module({
  providers: [NftService, PrismaService, TokenService, GraphQlcallerService],
  controllers: [NftController]
})
export class NftModule {}