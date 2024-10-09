import { InputType } from '@nestjs/graphql';
import { ORDERTYPE } from '@prisma/client';
import {
  IsEnum,
  IsNotEmpty,
  IsOptional,
  IsString,
  IsUUID,
} from 'class-validator';
import { makeTakeType } from 'src/constants/enums/order.enum';

@InputType()
export class CreateOrderDto {
  @IsString()
  @IsNotEmpty()
  sig: string;
}
