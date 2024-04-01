import {
  IsString,
  IsOptional,
  IsNotEmpty,
  IsDateString,
  IsEnum,
  IsArray,
  IsInt,
  ValidateNested,
  IsBoolean,
} from 'class-validator';
import { OffsetPaginationDto } from 'src/commons/definitions/OffsetPagination.input';
import { ProjectStat } from 'src/constants/enums/ProjectStat.enum';
import { Type } from 'class-transformer';

export class GetAllRoundDto extends OffsetPaginationDto {
  @IsOptional()
  name: string;
}

export class CreateRoundInforDto {
  @IsNotEmpty()
  @IsString()
  name: string;

  @IsNotEmpty()
  @IsString()
  type: string;

  @IsOptional()
  @IsString()
  description: string;
}

export class UpdateRoundInforDto extends CreateRoundInforDto {
  @IsOptional()
  @IsString()
  id: string;
}

export class FindAllProjectDto extends OffsetPaginationDto {
  @IsDateString()
  @IsOptional()
  start: Date;

  @IsDateString()
  @IsOptional()
  end: Date;

  @IsOptional()
  @IsEnum(ProjectStat)
  mode: ProjectStat;

  @IsString()
  @IsOptional()
  name: string;
}

export class CreateOrUpdateProjectDto {
  @IsString()
  @IsOptional()
  id: string;

  @IsString()
  @IsNotEmpty()
  name: string;

  @IsString()
  @IsNotEmpty()
  idOnchain: string;

  @IsString()
  @IsNotEmpty()
  banner: string;

  @IsString()
  @IsOptional()
  description: string;

  @IsString()
  @IsNotEmpty()
  organization: string;

  @IsString()
  @IsOptional()
  website: string;

  @IsArray()
  @IsOptional()
  details: string[];

  @IsString()
  @IsOptional()
  twitter: string;

  @IsString()
  @IsOptional()
  telegram: string;

  @IsString()
  @IsOptional()
  discord: string;

  @IsString()
  @IsOptional()
  facebook: string;

  @IsString()
  @IsOptional()
  instagram: string;

  @IsString()
  @IsNotEmpty()
  logo: string;

  @IsString()
  @IsNotEmpty()
  collectionAddress: string;

  @IsBoolean()
  @IsOptional()
  isActivated: boolean;

  @IsArray()
  @ValidateNested({ each: true })
  @Type(() => roundProjectDto)
  rounds: roundProjectDto[];
}

export class roundProjectDto {
  @IsString()
  @IsOptional()
  address: string;

  @IsDateString()
  @IsOptional()
  start: Date;

  @IsDateString()
  @IsOptional()
  end: Date;

  @IsString()
  @IsNotEmpty()
  roundId: string;

  @IsDateString()
  @IsOptional()
  stakeBefore: Date;

  @IsDateString()
  @IsOptional()
  claimableStart: Date;

  @IsString()
  @IsOptional()
  maxPerWallet: string;

  @IsString()
  @IsOptional()
  price: string;

  @IsString()
  @IsOptional()
  totalNftt: string;

  @IsString()
  @IsOptional()
  instruction: string;

  @IsArray()
  @IsOptional()
  claimableIds: string[];

  @IsString()
  @IsOptional()
  requiredStaking: string;
}
