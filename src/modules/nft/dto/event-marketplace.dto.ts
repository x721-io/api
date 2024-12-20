import { IsEnum, IsOptional, IsString } from 'class-validator';
import { OffsetPaginationDto } from 'src/commons/definitions/OffsetPagination.input';
import { SellStatus } from 'src/generated/graphql';
import { CONTRACT_TYPE } from '@prisma/client';

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
