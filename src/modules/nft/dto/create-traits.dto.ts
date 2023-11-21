import { IsOptional, IsString } from "class-validator";

export class CreateTraitDto {
    @IsString()
    trait_type: string;
    @IsString()
    value: string;
    @IsOptional()
    display_type: string;
  }
  