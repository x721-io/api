import {
  IsDate,
  IsDateString,
  IsEnum,
  IsOptional,
  IsString,
} from 'class-validator';

import { FindAllProjectDto } from './find-all-project.dto';

export class findProjectsUserSubscribe extends FindAllProjectDto {
  @IsOptional()
  @IsString()
  projectId: string;

  @IsString()
  @IsOptional()
  userId: string;
}
