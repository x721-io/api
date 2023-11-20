import { IsNotEmpty, IsString } from 'class-validator';

export class GetTokenIdDto {

  @IsString({message : 'ID NFT is invalid'})
  @IsNotEmpty({ message: 'Please Enter ID NFT' })
  collectionAddress : string;
}
