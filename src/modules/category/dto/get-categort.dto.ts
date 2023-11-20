import { InputType, Int, Field } from '@nestjs/graphql';
import { IsInt } from 'class-validator';


@InputType()
export class GetCategoryDto {
  @IsInt({message : 'Category ID Is Invalid'})
  id : string
}
