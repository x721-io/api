import { InputType, Int, Field } from '@nestjs/graphql';
import { IsNotEmpty, IsString , IsOptional, IsEnum } from 'class-validator';


@InputType()
export class CreateCategoryDto {
  @IsString({message : 'Category Name Is Invalid'})
  @IsNotEmpty({message : 'Please Enter Category Name'})
  name : string
}
