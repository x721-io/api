import { InputType } from '@nestjs/graphql';
import { IsOptional, IsNotEmpty, IsEnum } from 'class-validator';
// import { Type } from 'class-transformer';
import { TypeValidator } from '../../../constants/enums/Validator.enum';
@InputType()
export class ValidatorDto {
  @IsOptional()
  @IsEnum(TypeValidator)
  @IsNotEmpty()
  key: TypeValidator;

  @IsNotEmpty()
  @IsOptional()
  value: string;

  @IsOptional()
  collectionId: string;
}
