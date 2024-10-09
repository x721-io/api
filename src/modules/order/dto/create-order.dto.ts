import { InputType } from '@nestjs/graphql';
import { ORDERSTATUS, ORDERTYPE } from '@prisma/client';
import { stringList } from 'aws-sdk/clients/datapipeline';
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

  @IsEnum(makeTakeType)
  @IsNotEmpty()
  makeAssetType: makeTakeType;

  @IsString()
  @IsNotEmpty()
  makeAssetAddress: string;

  @IsString()
  @IsNotEmpty()
  makeAssetValue: string;

  @IsString()
  @IsNotEmpty()
  makeAssetId: string;

  @IsString()
  @IsNotEmpty()
  @IsOptional()
  takerAddress: string;

  @IsEnum(makeTakeType)
  @IsNotEmpty()
  takeAssetType: makeTakeType;

  @IsString()
  @IsNotEmpty()
  takeAssetAddress: string;

  @IsString()
  @IsNotEmpty()
  @IsOptional()
  takeAssetValue: string;

  @IsString()
  @IsNotEmpty()
  @IsOptional()
  takeAssetId: string;

  @IsString()
  @IsNotEmpty()
  salt: string;

  @IsNotEmpty()
  start: number;

  @IsNotEmpty()
  end: number;

  @IsString()
  @IsNotEmpty()
  dataOrder: string;

  @IsEnum(ORDERTYPE)
  @IsNotEmpty()
  orderType: ORDERTYPE;

  @IsString()
  @IsNotEmpty()
  tokenId: string;

  @IsString()
  @IsNotEmpty()
  collectionAddress: string;

  @IsString()
  @IsNotEmpty()
  price: string;

  @IsString()
  @IsNotEmpty()
  netPrice: string;
}
