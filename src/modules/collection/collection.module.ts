import { Module } from '@nestjs/common';
import { CollectionService } from './collection.service';
import { CollectionController } from './collection.controller';
import { PrismaService } from 'src/prisma/prisma.service';
import { TraitService } from '../nft/trait.service';
import { GetCollectionMarketData } from '../graph-qlcaller/getCollectionMarketData.service';
import { GraphQlcallerService } from '../graph-qlcaller/graph-qlcaller.service';
import { CollectionPriceService } from './collectionPrice.service';
import { UserService } from '../user/user.service';
import { ActivityService } from '../nft/activity.service';

@Module({
  providers: [
    CollectionService,
    PrismaService,
    TraitService,
    GetCollectionMarketData,
    GraphQlcallerService,
    CollectionPriceService,
    UserService,
    ActivityService,
  ],
  controllers: [CollectionController],
})
export class CollectionModule {}
