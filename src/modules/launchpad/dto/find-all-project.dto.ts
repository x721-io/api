import {
  IsDate,
  IsDateString,
  IsEnum,
  IsOptional,
  IsString,
} from 'class-validator';
import { ProjectStat } from 'src/constants/enums/ProjectStat.enum';

export class FindAllProjectDto {
  @IsDateString()
  @IsOptional()
  start: Date;

  @IsDateString()
  @IsOptional()
  end: Date;

  @IsOptional()
  @IsEnum(ProjectStat)
  mode: ProjectStat;
}
