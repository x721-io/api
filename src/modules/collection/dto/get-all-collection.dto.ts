import { InputType } from '@nestjs/graphql';
import { IsNotEmpty, IsString , IsOptional, IsEnum, IsNumberString } from 'class-validator';
import { TX_STATUS , CONTRACT_TYPE} from '@prisma/client';
import {TXSTATUS , CONTRACTTYPE} from '../entities/collection.entity'
import { Type } from 'class-transformer';

@InputType()
export class GetAllCollectionDto {
  // id: string;
  @IsNumberString()
  @Type(() => Number)
  @IsOptional()
  min: string;

  @IsNumberString()
  @Type(() => Number)
  @IsOptional()
  max: string;

  @IsString()
  @IsOptional()
  name: string;

}