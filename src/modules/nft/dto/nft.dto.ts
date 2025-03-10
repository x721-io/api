import { TX_STATUS, UserNFT } from '@prisma/client';
import { TraitEntity } from '../entities/trait.entity';

export class NftDto {
  id: string;
  name: string;
  txCreationHash: string;
  ipfsHash: string;
  traits: TraitEntity[];
  createdAt: Date;
  updatedAt: Date;
  status: TX_STATUS;
  tokenUri: string;
  creatorId: string;
  collectionId: string;
  owners?: Array<UserNFT>;
  sellInfo?: unknown;
  bidInfo?: unknown;
}
