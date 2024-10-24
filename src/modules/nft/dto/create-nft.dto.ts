import { TX_STATUS } from '@prisma/client';
import { IsNotEmpty, IsString, IsOptional, IsEnum } from 'class-validator';
import { CreateTraitDto } from './create-traits.dto';

export class CreateNftDto {
  @IsString({ message: 'ID NFT is invalid' })
  @IsNotEmpty({ message: 'Please Enter ID NFT' })
  id: string;

  @IsString({ message: 'Name NFT is invalid' })
  @IsNotEmpty({ message: 'Please Enter Name NFT' })
  name: string;

  @IsString({ message: 'IPFS Hash is invalid' })
  @IsNotEmpty({ message: 'Please Enter IPFS ' })
  ipfsHash: string;

  @IsString({ message: 'Image IPFS Hash is invalid' })
  @IsOptional()
  imageHash: string;

  // @IsNotEmpty({ message: 'Please Enter Traits ' })
  // traits: CreateTraitDto[];

  @IsString({ message: 'Token Uri is invalid' })
  @IsNotEmpty({ message: 'Please Enter Token Uri ' })
  tokenUri: string;

  @IsOptional()
  @IsString({ message: 'Transaction Hash is invalid' })
  @IsNotEmpty({ message: 'Please Enter Transaction Hash' })
  txCreationHash: string;

  @IsString({ message: 'Collection is invalid' })
  @IsNotEmpty({ message: 'Please Enter Collection ID' })
  collectionId: string;

  @IsString()
  @IsOptional()
  u2uId: string;
}
