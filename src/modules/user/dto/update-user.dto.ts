import { PartialType } from '@nestjs/mapped-types';
import { CreateUserDto } from './create-user.dto';
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

export class UpdateUserDto {
  @IsOptional()
  email: string;
  @IsOptional()
  username: string;
  @IsOptional()
  acceptedTerms: boolean;
  @IsOptional()
  bio: string;
  @IsOptional()
  facebookLink: string;
  @IsOptional()
  twitterLink: string;
  @IsOptional()
  telegramLink: string;
  @IsOptional()
  discordLink: string;
  @IsOptional()
  webURL: string;
  @IsOptional()
  avatar: string;
  @IsOptional()
  coverImage: string;
  @IsOptional()
  shortLink: string;
}
