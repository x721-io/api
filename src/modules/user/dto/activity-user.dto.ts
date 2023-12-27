import { IsEnum, IsOptional, ValidateNested, IsString } from 'class-validator';
import { OffsetPaginationDto } from 'src/commons/definitions/OffsetPagination.input';
import { LowercasePipe } from 'src/commons/pipe/LowerCase.pipe';
import { CONTRACT_TYPE } from '@prisma/client';
import { Type, Transform } from 'class-transformer';

export class GetActivityBase extends OffsetPaginationDto {
  @IsString()
  @Transform(({ value }) => new LowercasePipe().transform(value))
  from: string;

  @IsString()
  @Transform(({ value }) => new LowercasePipe().transform(value))
  to: string;

  @IsOptional()
  @IsEnum(CONTRACT_TYPE)
  type?: CONTRACT_TYPE;
}
