import { NFT, TX_STATUS } from '@prisma/client';
import { UserEntity } from 'src/modules/user/entities/user.entity';

export class NftEntity implements NFT {
  id: string;
  name: string;
  txCreationHash: string;
  ipfsHash: string;
  traits: string;
  createdAt: Date;
  updatedAt: Date;
  status: TX_STATUS;
  tokenUri: string;
  creatorId: string;
  collectionId: string;
  owners?: UserEntity[];
  constructor(partial: Partial<NFT>) {
    Object.assign(this, partial);
  }
  description: string;
  u2uId: string;
  imageHash: string;
}

export enum TXSTATUS {
  PENDING = 'PENDING',
  SUCCESS = 'SUCCESS',
  FAILED = 'FAILED',
}
