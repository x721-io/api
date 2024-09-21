import { IsEnum, IsOptional, IsString, IsNotEmpty } from 'class-validator';
import { OffsetPaginationDto } from 'src/commons/definitions/OffsetPagination.input';
import { LowercasePipe } from 'src/commons/pipe/LowerCase.pipe';
import { CONTRACT_TYPE } from '@prisma/client';
import { Type, Transform } from 'class-transformer';
import { EventType } from 'src/generated/graphql';

export class GetActivityBase extends OffsetPaginationDto {
  @IsString()
  @Transform(({ value }) => new LowercasePipe().transform(value))
  user: string;

  @IsOptional()
  @IsEnum(CONTRACT_TYPE)
  type?: CONTRACT_TYPE;
}

export class GetFollowingDto extends OffsetPaginationDto {
  @IsString()
  @IsOptional()
  search: string;
}

export class GetListBid extends OffsetPaginationDto {
  @IsEnum(EventType)
  @IsNotEmpty()
  event: EventType;
}
