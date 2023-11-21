import { IsEnum, IsNumber, IsOptional } from "class-validator";
import { QueryTraitDto } from "./query-trait.dto";
import { sellStatus } from "src/constants/enums/SellStatus.enum";

export class GetAllNftDto {

    @IsOptional()
    traits: QueryTraitDto[];

    @IsOptional()
    @IsEnum(sellStatus)
    sellStatus: sellStatus;

    @IsOptional()
    @IsNumber()
    price: number;

    @IsOptional()
    collectionAddress: string;

    @IsOptional()
    creatorAddress: string;

    @IsOptional()
    name: string;

}