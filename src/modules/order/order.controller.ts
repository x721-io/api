import {
  Controller,
  Get,
  Post,
  Body,
  Patch,
  Param,
  Delete,
  UseGuards,
  Query,
} from '@nestjs/common';
import { OrderService } from './order.service';
import {
  CreateBulkDto,
  CreateOfferDto,
  CreateSingleDto,
} from './dto/create-order.dto';
import { UpdateOrderDto } from './dto/update-order.dto';
import { AuthenticationGuard } from '../auth/guards/auth.guard';
import { GetCurrentUser } from 'src/decorators/get-current-user.decorator';
import { User } from '@prisma/client';
import {
  ActionOrderDto,
  GetListOfferDto,
  VerifyOfferDto,
  VerifyOrderDto,
  VerifyOrdersDto,
} from './dto/get-order.dto';
import { AuthenticationCustomizeGuard } from '../auth/guards/authCustomize.guard';

@Controller('order')
export class OrderController {
  constructor(private readonly orderService: OrderService) {}

  @Post('/single')
  @UseGuards(AuthenticationGuard)
  createSingle(
    @Body() createOrderDto: CreateSingleDto,
    @GetCurrentUser() user: User,
  ) {
    return this.orderService.createSingle(createOrderDto, user);
  }

  @Post('/verify')
  @UseGuards(AuthenticationGuard)
  verifyOrder(@Body() input: VerifyOrderDto, @GetCurrentUser() user: User) {
    return this.orderService.verifyOrder(input, user);
  }

  @Post('/verify-list')
  @UseGuards(AuthenticationGuard)
  verifyOrders(@Body() input: VerifyOrdersDto, @GetCurrentUser() user: User) {
    return this.orderService.verifyOrders(input, user);
  }

  @Post('/pending')
  @UseGuards(AuthenticationGuard)
  pendingOrder(@Body() input: ActionOrderDto, @GetCurrentUser() user: User) {
    return this.orderService.actionOrder(input, user);
  }

  @Post('/generate-bulk-data')
  @UseGuards(AuthenticationGuard)
  generateBulkData(
    @Body() createOrderBulkDto: CreateBulkDto,
    @GetCurrentUser() user: User,
  ) {
    return this.orderService.generateMerkleTree(createOrderBulkDto, user);
  }

  @Post('/bulk')
  @UseGuards(AuthenticationGuard)
  createBulk(
    @Body() createOrderBulkDto: CreateBulkDto,
    @GetCurrentUser() user: User,
  ) {
    return this.orderService.createBulk(createOrderBulkDto, user);
  }

  @Post('/offer-collection')
  @UseGuards(AuthenticationGuard)
  createOffer(
    @Body() createOfferDto: CreateOfferDto,
    @GetCurrentUser() user: User,
  ) {
    return this.orderService.createOffer(createOfferDto, user);
  }
  @Get('/offer-collections')
  @UseGuards(AuthenticationCustomizeGuard)
  getSweepOrders(
    @Query() query: GetListOfferDto,
    @GetCurrentUser() user: User,
  ) {
    return this.orderService.getListOffer(query, user);
  }

  @Get('/offer-detail')
  getDetailOffer(@Query() query: VerifyOfferDto) {
    return this.orderService.getDetailOffer(query);
  }

  @Post('/verify-offer')
  @UseGuards(AuthenticationGuard)
  verifyOffer(@Body() input: VerifyOfferDto, @GetCurrentUser() user: User) {
    return this.orderService.verifyOffer(input, user);
  }
}
