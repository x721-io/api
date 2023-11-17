import { InputType, Int, Field } from '@nestjs/graphql';
import { TX_STATUS } from '@prisma/client';
import { IsNotEmpty, IsString , IsOptional, IsEnum } from 'class-validator';

@InputType()
export class CreateNftDto {

  @IsString({message : 'ID NFT is invalid'})
  @IsNotEmpty({ message: 'Please Enter ID NFT' })
  id : string;

  @IsString({message : 'Name NFT is invalid'})
  @IsNotEmpty({ message: 'Please Enter Name NFT' })
  name : string;

  @IsString({message : 'IPFS Hash is invalid'})
  @IsNotEmpty({ message: 'Please Enter IPFS ' })
  ipfsHash : string;

  @IsString({message : 'Traits is invalid'})
  @IsNotEmpty({ message: 'Please Enter Traits ' })
  traits : string;

  @IsOptional()
  @IsEnum(TX_STATUS)
  status : TX_STATUS;

  @IsString({message : 'Token Uri is invalid'})
  @IsNotEmpty({ message: 'Please Enter Token Uri ' })
  tokenUri : string;

  @IsOptional()
  @IsString({message : 'Transaction Hash is invalid'})
  @IsNotEmpty({ message: 'Please Enter Transaction Hash' })
  txCreationHash : string;


  @IsString({message : 'User is invalid'})
  @IsNotEmpty({message : 'Please Enter User'})
  creatorId : string;

  @IsString({message : 'Collection is invalid'})
  @IsNotEmpty({message : 'Please Enter Collection ID'})
  collectionId : string;
}
