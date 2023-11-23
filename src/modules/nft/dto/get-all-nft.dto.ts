import { IsEnum, IsNumber, IsOptional } from "class-validator";
import { QueryTraitDto } from "./query-trait.dto";
import { sellStatus } from "src/constants/enums/SellStatus.enum";
import { OffsetPaginationDto } from "src/commons/definitions/OffsetPagination.input";
import { SellStatus } from "src/generated/graphql";

export class GetAllNftDto extends OffsetPaginationDto {

    @IsOptional()
    traits: QueryTraitDto[];

    @IsOptional()
    @IsEnum(SellStatus)
    sellStatus: SellStatus;

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