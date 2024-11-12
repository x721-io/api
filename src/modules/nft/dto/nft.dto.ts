import { TX_STATUS } from '@prisma/client';
import { TraitEntity } from '../entities/trait.entity';

export class NftDto {
  id: string;
  name: string;
  txCreationHash: string;
  traits: TraitEntity[];
  createdAt: Date;
  updatedAt: Date;
  status: TX_STATUS;
  tokenUri: string;
  creatorId: string;
  collectionId: string;
  // owners?: Array<UserNFT>;
  sellInfo?: unknown;
  bidInfo?: unknown;
  collection?: unknown;
  quoteToken?: string;
  creator?: unknown;
  quantity?: unknown;
}
