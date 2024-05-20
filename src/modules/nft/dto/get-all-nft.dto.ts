import { IsEnum, IsNumber, IsOptional, IsString, IsIn } from 'class-validator';
import { QueryTraitDto } from './query-trait.dto';
import { sellStatus } from 'src/constants/enums/SellStatus.enum';
import { OffsetPaginationDto } from 'src/commons/definitions/OffsetPagination.input';
import { SellStatus } from 'src/generated/graphql';
import { CONTRACT_TYPE } from '@prisma/client';

export class GetAllNftDto extends OffsetPaginationDto {
  @IsOptional()
  traits: QueryTraitDto[];

  @IsOptional()
  @IsEnum(SellStatus)
  sellStatus: SellStatus;

  @IsOptional()
  @IsString()
  priceMax: number;

  @IsOptional()
  @IsString()
  priceMin: number;

  @IsOptional()
  collectionAddress: string;

  @IsOptional()
  creatorAddress: string;

  @IsOptional()
  name: string;

  @IsOptional()
  owner: string;

  @IsOptional()
  from: string;

  @IsOptional()
  to: string;

  @IsOptional()
  quoteToken: string;

  @IsOptional()
  @IsEnum(CONTRACT_TYPE)
  type: CONTRACT_TYPE;

  @IsOptional()
  @IsIn(['time', 'price', 'all'])
  orderBy?: 'time' | 'price' | 'all' = 'all';
}
