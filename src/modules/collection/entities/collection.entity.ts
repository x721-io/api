import { Collection , TX_STATUS , CONTRACT_TYPE , Category , NFT , UserCollection} from '@prisma/client';

export class CollectionEntity implements Collection {
  id : string;
  txCreationHash : string;
  name : string;
  symbol: string;
  description : string;
  status : TX_STATUS;
  type : CONTRACT_TYPE;
  categoryId : number;
  // creators : string;
  createdAt : Date;
  updatedAt : Date;
  constructor(partial: Partial<Collection>) {
    Object.assign(this, partial);
  }
}
