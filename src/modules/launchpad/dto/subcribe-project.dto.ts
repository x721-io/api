import { InputType } from '@nestjs/graphql';
import { IsString } from 'class-validator';
@InputType()
export class SubcribeProjectDto {
  @IsString()
  projectId: string;

  @IsString()
  walletAddress: string;
}
