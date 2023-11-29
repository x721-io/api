import { IsEnum, IsOptional, ValidateNested } from 'class-validator';
import { OffsetPaginationDto } from 'src/commons/definitions/OffsetPagination.input';
import { GetEventMarketplaceQuery } from './get-event-marketplace-query.dto';
import { CONTRACT_TYPE } from '@prisma/client';
import { Type } from 'class-transformer';

export class GetEventBase extends OffsetPaginationDto {
  @IsOptional()
  @ValidateNested({ each: true })
  @Type(() => GetEventMarketplaceQuery)
  or?: GetEventMarketplaceQuery[];

  @IsOptional()
  @ValidateNested({ each: true })
  @Type(() => GetEventMarketplaceQuery)
  and?: GetEventMarketplaceQuery[];

  @IsOptional()
  @IsEnum(CONTRACT_TYPE)
  type?: CONTRACT_TYPE;
}
