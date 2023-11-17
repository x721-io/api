import { CreateNftDto } from './create-nft.dto';
import { InputType, Field, Int, PartialType } from '@nestjs/graphql';

@InputType()
export class UpdateNftDto extends PartialType(CreateNftDto) {
  // @Field(() => Int)
  // id: number;
}
