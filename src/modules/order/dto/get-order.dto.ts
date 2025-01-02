import { InputType } from '@nestjs/graphql';
import { ORDERSTATUS, ORDERTYPE } from '@prisma/client';
import { Type } from 'class-transformer';
import {
  IsEnum,
  IsNotEmpty,
  IsOptional,
  IsString,
  IsUUID,
  IsNumber,
  ArrayNotEmpty,
  ValidateNested,
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

@InputType()
export class VerifyOrdersDto {
  @ArrayNotEmpty()
  @ValidateNested({ each: true })
  @Type(() => VerifyOrderDto)
  orders: VerifyOrderDto[];
}

export class ActionOrderDto {
  @IsString()
  @IsNotEmpty()
  tx: string;
}
