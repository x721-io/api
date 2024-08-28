import { InputType } from '@nestjs/graphql';
import { IsNotEmpty, IsString, IsOptional, IsEnum } from 'class-validator';
import { TX_STATUS, CONTRACT_TYPE } from '@prisma/client';
import { CreationMode } from 'src/constants/enums/creation.enum';

@InputType()
export class CreateCollectionDto {
  // id: string;

  @IsString({ message: 'Transaction Hash is invalid' })
  @IsNotEmpty({ message: 'Please Enter Transaction Hash' })
  txCreationHash: string;

  @IsString({ message: 'Short url is invalid' })
  @IsOptional()
  shortUrl: string;

  @IsString({ message: 'Name Collection is invalid' })
  @IsNotEmpty({ message: 'Please Enter Name Collection' })
  name: string;

  @IsString({ message: 'Symbol Collection is invalid' })
  @IsNotEmpty({ message: 'Please Enter Symbol Collection' })
  symbol: string;

  @IsOptional()
  description: string;

  @IsOptional()
  metadata: string;

  @IsOptional()
  @IsEnum(TX_STATUS)
  status: TX_STATUS;

  @IsEnum(CONTRACT_TYPE)
  type: CONTRACT_TYPE;

  @IsOptional()
  categoryId: number;

  @IsOptional()
  @IsNotEmpty({ message: 'Please Enter User ID' })
  creators: string;

  @IsOptional()
  @IsString()
  coverImage: string;

  @IsOptional()
  @IsString()
  avatar: string;

  @IsOptional()
  @IsEnum(CreationMode)
  modeCreate: CreationMode;

  @IsString()
  @IsOptional()
  creatorAddress: string;
}
