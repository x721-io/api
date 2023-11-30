import { ObjectType, Field, Int } from '@nestjs/graphql';

@ObjectType()
export class Validator {
  @Field(() => Int, { description: 'Example field (placeholder)' })
  exampleField: number;
}
