import { PartialType } from '@nestjs/mapped-types';
import { CreateUserDto } from './create-user.dto';
import { IsNotEmpty, IsString, IsOptional, MaxLength } from 'class-validator';

export class UpdateUserDto {
  email: string;
  @IsString()
  @IsNotEmpty()
  @IsOptional()
  @MaxLength(25, {
    message: 'Username is too long, maximum length is 25 characters',
  })
  username: string;
  acceptedTerms: boolean;
  @IsOptional()
  @IsString()
  @MaxLength(1200, {
    message: 'Bio is too long, maximum length is 1200 characters',
  })
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
  @MaxLength(25, {
    message: 'Short link is too long, maximum length is 25 characters',
  })
  shortLink: string;
}
