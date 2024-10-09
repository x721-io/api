import {
  Controller,
  Get,
  Post,
  Body,
  Patch,
  Param,
  Delete,
  UseGuards,
} from '@nestjs/common';
import { OrderService } from './order.service';
import { CreateOrderDto } from './dto/create-order.dto';
import { UpdateOrderDto } from './dto/update-order.dto';
import { AuthenticationGuard } from '../auth/guards/auth.guard';
import { GetCurrentUser } from 'src/decorators/get-current-user.decorator';
import { User } from '@prisma/client';

@Controller('order')
export class OrderController {
  constructor(private readonly orderService: OrderService) {}

  @Post()
  @UseGuards(AuthenticationGuard)
  create(@Body() createOrderDto: CreateOrderDto, @GetCurrentUser() user: User) {
    return this.orderService.create(createOrderDto, user);
  }
}
