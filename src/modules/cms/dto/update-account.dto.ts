import {
  IsString,
  IsOptional,
  IsArray,
  IsEmail,
  IsNotEmpty,
} from 'class-validator';
import OtherCommon from 'src/commons/Other.common';

export class UpdateAccountDto {
  @IsString()
  @IsOptional()
  avatar: string;

  @IsString()
  @IsOptional()
  @IsEmail()
  email: string;

  @IsArray()
  @IsOptional()
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
  @IsOptional()
  fullName: string;
}

export class UpdatePasswordDto {
  @IsString()
  @IsNotEmpty()
  currentPassword: string;

  @IsString()
  @IsNotEmpty()
  newPassword: string;
}

export class ResetPasswordDtop {
  @IsString()
  @IsNotEmpty()
  id: string;

  @IsString()
  @IsNotEmpty()
  newPassword: string;
}

export class UpdateRolesDto {
  @IsString()
  @IsNotEmpty()
  id: string;

  @IsArray()
  @IsString({ each: true }) // Optional: To check if each element of the array is a string
  @IsNotEmpty()
  @OtherCommon.IsValidRoles({ message: 'Roles must be valid roles' })
  roles: string[];
}
