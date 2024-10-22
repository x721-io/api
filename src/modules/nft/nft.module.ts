import { Module } from '@nestjs/common';
import { NftService } from './nft.service';
import { NftController } from './nft.controller';
import { PrismaService } from 'src/prisma/prisma.service';
import { TokenService } from './token.service';
import { GraphQlcallerService } from '../graph-qlcaller/graph-qlcaller.service';
import { MarketplaceService } from './nft-marketplace.service';
import { ValidatorService } from '../validator/validator.service';
import { ActivityService } from './activity.service';
import { CollectionPriceService } from '../collection/collectionPrice.service';
import { GetCollectionMarketData } from '../graph-qlcaller/getCollectionMarketData.service';
import { NFTHepler } from './helper/nft-helper.service';
import { UserService } from '../user/user.service';

@Module({
  providers: [
    NftService,
    PrismaService,
    TokenService,
    GraphQlcallerService,
    MarketplaceService,
    ValidatorService,
    ActivityService,
    CollectionPriceService,
    GetCollectionMarketData,
    NFTHepler,
    UserService,
    ActivityService,
  ],
  controllers: [NftController],
})
export class NftModule {}
