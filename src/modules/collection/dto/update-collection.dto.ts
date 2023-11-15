import { CreateCollectionDto } from './create-collection.dto';
import { InputType, Field, Int, PartialType } from '@nestjs/graphql';
import { TX_STATUS , CONTRACT_TYPE} from '@prisma/client';

@InputType()
export class UpdateCollectionDto extends PartialType(CreateCollectionDto) {
  txCreationHash: string;
  name : string;
  symbol : string;
  description : string;
  status : TX_STATUS;
  type : CONTRACT_TYPE;
  categoryId : number;
  creators : string;
}
