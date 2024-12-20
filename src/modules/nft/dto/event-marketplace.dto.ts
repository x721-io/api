import { IsArray, IsEnum, IsOptional, IsString } from 'class-validator';
import { OffsetPaginationDto } from 'src/commons/definitions/OffsetPagination.input';
import { SellStatus } from 'src/generated/graphql';
import { CONTRACT_TYPE, ORDERSTATUS, ORDERTYPE } from '@prisma/client';

export class GetEventMarketplace extends OffsetPaginationDto {
  @IsOptional()
  @IsString()
  nftId: string;
  @IsString()
  @IsOptional()
  from?: string;
  @IsString()
  @IsOptional()
  to?: string;
  @IsString()
  @IsOptional()
  quoteToken?: string;

  @IsOptional()
  @IsEnum(SellStatus)
  event?: SellStatus;

  @IsOptional()
  @IsEnum(CONTRACT_TYPE)
  type?: CONTRACT_TYPE;

  contractAddress: string;
}

export class GetEventOrder extends OffsetPaginationDto {
  @IsOptional()
  @IsString()
  nftId: string;

  @IsString()
  @IsOptional()
  from?: string;

  @IsString()
  @IsOptional()
  to?: string;

  @IsString()
  @IsOptional()
  quoteToken?: string;

  @IsOptional()
  @IsEnum(ORDERSTATUS)
  status?: ORDERSTATUS;

  @IsOptional()
  @IsArray() // Add this to specify it's an array
  @IsEnum(ORDERTYPE, { each: true }) // Validate each item in the array
  event?: ORDERTYPE[];

  @IsString()
  @IsOptional()
  contractAddress: string;
}
