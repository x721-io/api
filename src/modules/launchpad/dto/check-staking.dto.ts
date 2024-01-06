import { IsOptional } from 'class-validator';

export class CheckStakingDto {
  @IsOptional()
  projectId: string;
}
