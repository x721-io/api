import { InputType } from '@nestjs/graphql';
import { IsString, IsOptional } from 'class-validator';
import { Type } from 'class-transformer';

@InputType()
export class GetAllCollectionDto {
  // id: string;
  @Type(() => Number)
  @IsOptional()
  min: string;

  @Type(() => Number)
  @IsOptional()
  max: string;

  @IsString()
  @IsOptional()
  name: string;

  @IsString()
  @IsOptional()
  creatorAddresses: string;
}
