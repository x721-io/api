import {
  IsString,
  IsOptional,
  IsNotEmpty,
  IsBoolean,
  IsEnum,
  IsDateString,
} from 'class-validator';
import { EventType } from 'src/generated/graphql';

export class GetSummaryDto {
  @IsDateString()
  @IsOptional()
  start: string;

  @IsDateString()
  @IsOptional()
  end: string;
}
