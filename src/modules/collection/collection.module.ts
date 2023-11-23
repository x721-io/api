import { Module } from '@nestjs/common';
import { CollectionService } from './collection.service';
import { CollectionController } from './collection.controller';
import { PrismaService } from 'src/prisma/prisma.service';
import { TraitService } from '../nft/trait.service';
import { GetCollectionMarketData } from '../graph-qlcaller/getCollectionMarketData.service';

@Module({
  providers: [CollectionService , PrismaService, TraitService, GetCollectionMarketData],
  controllers: [CollectionController]
})
export class CollectionModule {}
