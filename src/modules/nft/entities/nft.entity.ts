import { ObjectType, Field, Int } from '@nestjs/graphql';
import {NFT, TX_STATUS, CONTRACT_TYPE } from "@prisma/client";


@ObjectType()
export class NftEntity implements NFT{
  id : string;
  name : string;
  txCreationHash : string;
  ipfsHash : string;
  traits : string;
  createdAt : Date;
  updatedAt : Date;
  status : TX_STATUS;
  tokenUri : string;
  creatorId : string;
  collectionId : string;
  constructor(partial: Partial<NFT>) {
    Object.assign(this, partial);
  }
}
 
export enum TXSTATUS {
  PENDING = "PENDING",
  SUCCESS = "SUCCESS",
  FAILED = "FAILED"
}
