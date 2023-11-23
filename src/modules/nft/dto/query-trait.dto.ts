import { IsOptional, IsString } from "class-validator";

export class QueryTraitDto {
    @IsString()
    trait_type: string;
    @IsOptional()
    value: string;
    @IsOptional()
    display_type: string;
  }
  