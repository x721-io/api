import { InputType } from '@nestjs/graphql';
import {
  IsNotEmpty,
  IsString,
  IsOptional,
  IsEnum,
  IsBoolean,
  ValidateNested,
} from 'class-validator';
import { TX_STATUS, CONTRACT_TYPE } from '@prisma/client';

import { CreateCollectionDto } from 'src/modules/collection/dto/create-collection.dto';
import { Type } from 'class-transformer';

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

export class Category {
  @IsNotEmpty()
  @IsString()
  id: string;

  @IsNotEmpty()
  @IsString()
  extract: string;

  @IsNotEmpty()
  @IsString()
  name: string;
}

export class Game {
  @IsString()
  @IsNotEmpty()
  name: string;

  @ValidateNested()
  @Type(() => Category)
  category: Category[];
}

export class WebhookCollectionDto extends CreateCollectionDto {
  @IsOptional()
  @IsBoolean()
  flagExternal: boolean;

  @IsOptional()
  @IsString()
  subgraphUrl: string;

  meta: JSON;

  @IsNotEmpty()
  @IsString()
  address: string;

  @IsNotEmpty()
  @IsString()
  gameId: string;
}
