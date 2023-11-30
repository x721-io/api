import { CreateValidatorDto } from './create-validator.dto';
import { InputType, Field, Int, PartialType } from '@nestjs/graphql';

@InputType()
export class UpdateValidatorDto extends PartialType(CreateValidatorDto) {
  @Field(() => Int)
  id: number;
}
