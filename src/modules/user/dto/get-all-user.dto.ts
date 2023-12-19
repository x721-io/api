import { CursorPagination } from 'src/commons/definitions/CursorPagition.input';
import { OffsetPaginationDto } from 'src/commons/definitions/OffsetPagination.input';
import {
  IsNotEmpty,
  IsString,
  IsOptional,
  IsEnum,
  IsNumber,
  IsNumberString,
  Min,
  IsBoolean,
  IsDefined,
} from 'class-validator';

export class GetAllUser extends OffsetPaginationDto {
  @IsOptional()
  search: string;
}
