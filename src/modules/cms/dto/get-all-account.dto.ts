import { IsOptional } from 'class-validator';
import { OffsetPaginationDto } from 'src/commons/definitions/OffsetPagination.input';

export class GetAllAccountDto extends OffsetPaginationDto {
  @IsOptional()
  username: string;
}
