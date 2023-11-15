import { InputType, Int, Field } from '@nestjs/graphql';
import { IsNotEmpty } from 'class-validator';
import { TX_STATUS , CONTRACT_TYPE} from '@prisma/client';

@InputType()
export class CreateCollectionDto {
  id: string;
  txCreationHash: string;
  name : string;
  symbol : string;
  description : string;
  status : TX_STATUS;
  type : CONTRACT_TYPE;
  categoryId : number;
  creators : string;
}