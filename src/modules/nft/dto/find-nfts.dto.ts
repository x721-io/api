import {
  IsEnum,
  IsIn,
  IsOptional,
  IsString,
  ValidateNested,
} from 'class-validator';
import { Type } from 'class-transformer';
import { ApiProperty } from '@nestjs/swagger';
import { OffsetPaginationDto } from 'src/commons/definitions/OffsetPagination.input';
import { ORDERSTATUS, ORDERTYPE } from '@prisma/client';
import { SellStatus } from 'src/generated/graphql';

export class MetadataFilterDto {
  @ApiProperty({
    required: false,
    description: 'Filter by metadata name',
  })
  @IsOptional()
  @IsString()
  name?: string;

  @ApiProperty({
    required: false,
    description: 'Filter by category ID',
  })
  @IsOptional()
  @IsString()
  categoryId?: string;

  @ApiProperty({
    required: false,
    description: 'Filter by category name',
  })
  @IsOptional()
  @IsString()
  categoryName?: string;
}

export class CollectionFilterDto {
  @ApiProperty({
    required: false,
    type: MetadataFilterDto,
    description: 'Filter by collection metadata fields',
  })
  @IsOptional()
  @ValidateNested()
  @Type(() => MetadataFilterDto)
  metadata?: MetadataFilterDto;

  @ApiProperty({
    required: false,
    description: 'Filter by collection name',
  })
  @IsOptional()
  @IsString()
  name?: string;

  @ApiProperty({
    required: false,
    description: 'Filter by collection symbol',
  })
  @IsOptional()
  @IsString()
  symbol?: string;

  @ApiProperty({
    required: false,
    description: 'Filter by collection address',
  })
  @IsOptional()
  @IsString()
  address?: string;
}

export class FindNftsDto extends OffsetPaginationDto {
  @ApiProperty({
    required: false,
    description: 'Filter by NFT name',
  })
  @IsOptional()
  @IsString()
  nftName?: string;

  @ApiProperty({
    required: false,
    type: CollectionFilterDto,
    description: 'Collection filters',
  })
  @IsOptional()
  @ValidateNested()
  @Type(() => CollectionFilterDto)
  collection?: CollectionFilterDto;

  @ApiProperty({
    required: false,
    enum: ['time', 'price', 'all'],
    default: 'all',
    description: 'type sort',
  })
  @IsOptional()
  @IsIn(['time', 'price', 'all'])
  orderBy?: 'time' | 'price' | 'all' = 'all';

  @ApiProperty({
    required: false,
    enum: ['OPEN', 'PENDING', 'CANCELLED', 'FILLED'],
    description: 'Status Order',
  })
  @IsOptional()
  @IsEnum(ORDERSTATUS)
  orderStatus: ORDERSTATUS;

  @ApiProperty({
    required: false,
    enum: ['BID', 'BULK', 'SINGLE'],
    description: 'Order Type',
  })
  @IsOptional()
  @IsEnum(ORDERTYPE)
  orderType: ORDERTYPE;

  @ApiProperty({
    required: false,
    description: 'Filter Prices Max',
  })
  @IsOptional()
  @IsString()
  priceMax: number;

  @ApiProperty({
    required: false,
    description: 'Filter Prices Min',
  })
  @IsOptional()
  @IsString()
  priceMin: number;

  @ApiProperty({
    required: false,
    description: 'Quote Token Sell',
  })
  @IsOptional()
  quoteToken: string;

  // @ApiProperty({
  //   required: false,
  //   enum: ['BID', 'BULK', 'SINGLE'],
  //   description: 'Order Type',
  // })
  @IsOptional()
  @IsEnum(SellStatus)
  sellStatus: SellStatus;
}
