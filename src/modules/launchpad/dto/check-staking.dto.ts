import { PartialType } from '@nestjs/mapped-types';
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

export class CheckStakingDto {
  @IsOptional()
  projectId: string;
}
