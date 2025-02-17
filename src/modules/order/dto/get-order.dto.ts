import { extend, InputType } from '@nestjs/graphql';
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
import { OffsetPaginationDto } from 'src/commons/definitions/OffsetPagination.input';
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

export class GetListOfferDto extends OffsetPaginationDto {
  @IsOptional()
  @IsString()
  collection: string;

  @IsOptional()
  @IsString()
  search: string;
}

@InputType()
export class VerifyOfferDto {
  @IsString()
  @IsNotEmpty()
  sig: string;

  @IsString()
  @IsNotEmpty()
  index: string;
}
