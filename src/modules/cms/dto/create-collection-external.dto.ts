import { InputType } from '@nestjs/graphql';
import {
  IsNotEmpty,
  IsString,
  IsOptional,
  IsEnum,
  IsBoolean,
} from 'class-validator';
import { TX_STATUS, CONTRACT_TYPE } from '@prisma/client';

import { CreateCollectionDto } from 'src/modules/collection/dto/create-collection.dto';

export class CreateCollectionExternalDto extends CreateCollectionDto {
  @IsOptional()
  @IsBoolean()
  flagExternal: boolean;

  @IsOptional()
  @IsString()
  subgraphUrl: string;

  @IsOptional()
  @IsString()
  userId: string;
}
