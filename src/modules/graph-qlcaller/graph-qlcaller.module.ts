import { Module } from '@nestjs/common';
import { GraphQlcallerService } from './graph-qlcaller.service';
// import { GraphQlcallerController } from './graph-qlcaller.controller';
import { GraphQLModule } from '@nestjs/graphql';
import { GetCollectionMarketData } from './getCollectionMarketData.service';

@Module({
  // controllers: [GraphQlcallerController],
  providers: [GraphQlcallerService, GetCollectionMarketData],
})
export class GraphQlcallerModule {}
