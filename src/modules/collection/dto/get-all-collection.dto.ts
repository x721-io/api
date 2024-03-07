import { IsString, IsOptional, IsIn } from 'class-validator';
import { Type } from 'class-transformer';
import { OffsetPaginationDto } from 'src/commons/definitions/OffsetPagination.input';

export class GetAllCollectionDto extends OffsetPaginationDto {
  // id: string;
  @IsString()
  @IsOptional()
  min: string;

  @IsString()
  @IsOptional()
  max: string;

  @IsString()
  @IsOptional()
  name: string;

  @IsString()
  @IsOptional()
  creatorAddresses: string;

  @IsOptional()
  @IsIn(['time', 'price'])
  orderBy?: 'time' | 'price' = 'time';
}
