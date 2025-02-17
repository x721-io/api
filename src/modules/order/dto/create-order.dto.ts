import { InputType } from '@nestjs/graphql';
import { ORDERSTATUS, ORDERTYPE } from '@prisma/client';
import { stringList } from 'aws-sdk/clients/datapipeline';
import {
  IsArray,
  IsEnum,
  IsIn,
  IsNotEmpty,
  IsNumber,
  IsOptional,
  IsString,
  IsUUID,
  ValidateIf,
} from 'class-validator';
import { makeTakeType } from 'src/constants/enums/order.enum';

@InputType()
export class CreateSingleDto {
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
  taker: string;

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

  @IsEnum(ORDERTYPE)
  @IsNotEmpty()
  orderType: ORDERTYPE;

  @IsString()
  @IsNotEmpty()
  price: string;

  @IsString()
  @IsNotEmpty()
  netPrice: string;

  @IsNotEmpty()
  @IsNumber()
  index: number;

  @IsString()
  @IsOptional()
  root: string;

  @IsArray() // Ensure that proof is an array of strings
  @IsString({ each: true }) // Validate that each element in the array is a string
  @IsOptional()
  proof: string[];
}

export class CreateBulkDto {
  orders: CreateSingleDto[];
}

export class ItemBulkDto extends CreateSingleDto {
  @IsString()
  @IsOptional()
  sig: string;
}

@InputType()
export class CreateOfferDto {
  @IsString()
  @IsNotEmpty()
  sig: string;

  @IsNotEmpty()
  @IsNumber()
  index: number;

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

  @IsIn(['BID_COLLECTION'])
  @IsNotEmpty()
  offerType: ORDERTYPE;

  @IsString()
  @IsNotEmpty()
  price: string;

  @IsString()
  @IsNotEmpty()
  netPrice: string;
}
