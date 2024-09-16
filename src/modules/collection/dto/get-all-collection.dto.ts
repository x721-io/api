import { IsString, IsOptional, IsIn, IsEnum } from 'class-validator';
import { Type } from 'class-transformer';
import { OffsetPaginationDto } from 'src/commons/definitions/OffsetPagination.input';
import { SourceType } from 'src/constants/enums/Source.enum';
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
  @IsIn(['time', 'price', 'all'])
  orderBy?: 'time' | 'price' | 'all' = 'all';

  @IsOptional()
  @IsEnum(SourceType)
  source?: SourceType;
}
