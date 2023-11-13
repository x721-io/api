import { Type } from "class-transformer";
import { IsIn, IsInt, IsOptional, Min } from "class-validator";

export class OffsetPaginationDto {
    @IsOptional()
    @IsInt()
    @Type(() => Number)
    @Min(0)
    page?: number = 0;
  
    @IsOptional()
    @IsInt()
    @Type(() => Number)
    @Min(1)
    limit?: number = 10;
  
    @IsOptional()
    @IsIn(['asc', 'desc'])
    order?: 'asc' | 'desc' = 'asc';
  
    // Add any other common pagination-related properties or methods here.
  }
  