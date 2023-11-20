import { CreateCategoryDto } from './create-category.dto';
import { InputType, Field, Int, PartialType } from '@nestjs/graphql';
import { IsNotEmpty, IsString , IsOptional, IsEnum } from 'class-validator';


@InputType()
export class UpdateCategoryDto extends PartialType(CreateCategoryDto) {
  @IsString({message : 'Category Name Is Invalid'})
  @IsNotEmpty({message : 'Please Enter Category Name'})
  name : string
}
