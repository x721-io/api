import { IsDate, IsDateString, IsOptional } from 'class-validator';

export class FindAllProjectDto {
  @IsDateString()
  @IsOptional()
  start: Date;

  @IsDateString()
  @IsOptional()
  end: Date;
}
