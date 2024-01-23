import { InputType } from '@nestjs/graphql';
import { IsString, IsOptional } from 'class-validator';

@InputType()
export class FollowCollectionDto {
  @IsString()
  @IsOptional()
  collectionId: string;
}
