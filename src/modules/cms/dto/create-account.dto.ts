import { InputType } from '@nestjs/graphql';
import {
  IsString,
  IsOptional,
  IsArray,
  IsEmail,
  IsNotEmpty,
} from 'class-validator';
import { SignInDto } from './sign-in.dto';
import OtherCommon from 'src/commons/Other.common';

@InputType()
export class CreateAccountDto extends SignInDto {
  @IsString()
  @IsOptional()
  id: string;

  @IsString()
  @IsOptional()
  avatar: string;

  @IsString()
  @IsOptional()
  @IsEmail()
  email: string;

  @IsArray()
  @IsString({ each: true }) // Optional: To check if each element of the array is a string
  @IsNotEmpty()
  @OtherCommon.IsValidRoles({ message: 'Roles must be valid roles' })
  roles: string[];

  @IsString()
  @IsOptional()
  twitterLink: string;

  @IsString()
  @IsOptional()
  telegramLink: string;

  @IsString()
  @IsOptional()
  phone: string;

  @IsString()
  @IsNotEmpty()
  username: string;

  @IsString()
  @IsNotEmpty()
  password: string;

  @IsString()
  @IsOptional()
  fullName: string;
}
