import { InputType, Int, Field } from '@nestjs/graphql';

@InputType()
export class CreateValidatorDto {
  @Field(() => Int, { description: 'Example field (placeholder)' })
  exampleField: number;
}
