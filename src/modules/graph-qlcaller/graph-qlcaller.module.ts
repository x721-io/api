import { Module } from '@nestjs/common';
import { GraphQlcallerService } from './graph-qlcaller.service';
import { GraphQlcallerController } from './graph-qlcaller.controller';
import { GraphQLModule } from '@nestjs/graphql';

@Module({
  controllers: [GraphQlcallerController],
  providers: [GraphQlcallerService],
})
export class GraphQlcallerModule {}
