import { InputType } from '@nestjs/graphql';
import { IsNotEmpty, IsString, IsOptional, IsEnum } from 'class-validator';
import { TX_STATUS, CONTRACT_TYPE } from '@prisma/client';

@InputType()
export class UpdateCollectionDto {
  @IsOptional()
  @IsString({ message: 'Transaction Hash is invalid' })
  @IsNotEmpty({ message: 'Please Enter Transaction Hash' })
  txCreationHash: string;

  @IsOptional()
  @IsString({ message: 'Name Collection is invalid' })
  @IsNotEmpty({ message: 'Please Enter Name Collection' })
  name: string;

  @IsOptional()
  @IsString({ message: 'Symbol Collection is invalid' })
  @IsNotEmpty({ message: 'Please Enter Symbol Collection' })
  symbol: string;

  @IsString({ message: 'Short url is invalid' })
  @IsOptional()
  shortUrl: string;

  @IsOptional()
  description: string;

  @IsOptional()
  @IsEnum(TX_STATUS)
  status: TX_STATUS;

  @IsOptional()
  @IsEnum(CONTRACT_TYPE)
  type: CONTRACT_TYPE;

  @IsOptional()
  categoryId: number;

  @IsOptional()
  creators: string;

  @IsOptional()
  coverImage: string;
}
