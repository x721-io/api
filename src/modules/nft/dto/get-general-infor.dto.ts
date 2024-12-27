import {
  IsEnum,
  IsNotEmpty,
  IsNumber,
  IsOptional,
  IsString,
} from 'class-validator';
import { GeneralInfor } from 'src/constants/enums/GeneralInfor.enum';
import { SellStatus } from 'src/generated/graphql';
import { OffsetPaginationDto } from 'src/commons/definitions/OffsetPagination.input';
export class GetGeneralInforDto extends OffsetPaginationDto {
  @IsOptional()
  owner: string;

  @IsOptional()
  collectionAddress: string;

  @IsOptional()
  creatorAddress: string;

  @IsOptional()
  @IsEnum(SellStatus)
  sellStatus: SellStatus;

  @IsEnum(GeneralInfor)
  mode: GeneralInfor;
}

export class GetGeneralInforAllDto {
  @IsString()
  @IsNotEmpty()
  owner: string;
}
