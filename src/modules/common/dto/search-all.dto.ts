import { IsEnum, IsString } from 'class-validator';
import { SearchAllType } from 'src/constants/searchType.enum';

export class SearchAllDto {
  @IsString()
  text: string;

  @IsEnum(SearchAllType)
  mode: SearchAllType = SearchAllType.COLLECTION;
}
