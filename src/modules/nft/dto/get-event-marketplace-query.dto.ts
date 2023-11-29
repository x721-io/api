import { Transform } from 'class-transformer';
import { IsEnum, IsOptional, IsString } from 'class-validator';
import { LowercasePipe } from 'src/commons/pipe/LowerCase.pipe';
import { SellStatus } from 'src/generated/graphql';

export class GetEventMarketplaceQuery {
  @IsOptional()
  @IsString()
  nftId?: string;

  @IsString()
  @IsOptional()
  @Transform(({ value }) => new LowercasePipe().transform(value))
  from?: string;

  @IsString()
  @IsOptional()
  @Transform(({ value }) => new LowercasePipe().transform(value))
  to?: string;

  @IsString()
  @IsOptional()
  quoteToken?: string;

  @IsOptional()
  @IsEnum(SellStatus)
  event?: SellStatus;
}
