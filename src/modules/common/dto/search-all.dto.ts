import { IsEnum, IsString } from 'class-validator';
import { SearchAllType } from 'src/constants/searchType.enum';
import { OffsetPaginationDto } from 'src/commons/definitions/OffsetPagination.input';

export class SearchAllDto extends OffsetPaginationDto {
  @IsString()
  text: string;

  @IsEnum(SearchAllType)
  mode: SearchAllType = SearchAllType.COLLECTION;
}
