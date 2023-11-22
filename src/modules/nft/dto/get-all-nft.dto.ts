import { IsEnum, IsNumber, IsOptional } from "class-validator";
import { QueryTraitDto } from "./query-trait.dto";
import { sellStatus } from "src/constants/enums/SellStatus.enum";
import { OffsetPaginationDto } from "src/commons/definitions/OffsetPagination.input";

export class GetAllNftDto extends OffsetPaginationDto {

    @IsOptional()
    traits: QueryTraitDto[];

    @IsOptional()
    @IsEnum(sellStatus)
    sellStatus: sellStatus;

    @IsOptional()
    @IsNumber()
    priceMax: number;

    @IsOptional()
    @IsNumber()
    priceMin: number;

    @IsOptional()
    collectionAddress: string;

    @IsOptional()
    creatorAddress: string;

    @IsOptional()
    name: string;

}