import { InputType } from '@nestjs/graphql';
import { ORDERSTATUS, ORDERTYPE } from '@prisma/client';
import {
  IsEnum,
  IsNotEmpty,
  IsOptional,
  IsString,
  IsUUID,
  IsNumber,
} from 'class-validator';
import { makeTakeType } from 'src/constants/enums/order.enum';

@InputType()
export class VerifyOrderDto {
  @IsString()
  @IsNotEmpty()
  sig: string;

  @IsNumber()
  @IsNotEmpty()
  index: number;
}

export class ActionOrderDto {
  @IsString()
  @IsNotEmpty()
  tx: string;
}
