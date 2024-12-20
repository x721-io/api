import { Module } from '@nestjs/common';
import { OrderService } from './order.service';
import { OrderController } from './order.controller';
import { PrismaService } from 'src/prisma/prisma.service';
import { GraphQlcallerService } from '../graph-qlcaller/graph-qlcaller.service';
import { UserService } from '../user/user.service';
import { ActivityService } from '../nft/activity.service';
@Module({
  controllers: [OrderController],
  providers: [
    OrderService,
    PrismaService,
    GraphQlcallerService,
    UserService,
    ActivityService,
  ],
})
export class OrderModule {}
