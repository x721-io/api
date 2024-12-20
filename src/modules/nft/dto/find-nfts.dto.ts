import { IsOptional, IsString, ValidateNested } from 'class-validator';
import { Type } from 'class-transformer';
import { ApiProperty } from '@nestjs/swagger';
import { OffsetPaginationDto } from 'src/commons/definitions/OffsetPagination.input';

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
    enum: ['asc', 'desc'],
    default: 'desc',
    description: 'Sort by creation date',
  })
  @IsOptional()
  @IsString()
  orderBy?: 'asc' | 'desc' = 'desc';
}
