import { CursorPagination } from 'src/commons/definitions/CursorPagition.input';
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

export class GetAllUser extends CursorPagination {
  @IsOptional()
  search: string;
}
