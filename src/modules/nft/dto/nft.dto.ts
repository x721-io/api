import {TX_STATUS , UserNFT} from "@prisma/client";

export class NftDto {
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
  owners? : Array<UserNFT>
}