import { IsIn, IsNumber, IsOptional, IsString, Min } from "class-validator";

export class CursorPagination {
    @IsOptional()
    @IsNumber()
    @Min(1)
    limit?: number;

    @IsOptional()
    @IsString()
    cursor?: string;

    @IsOptional()
    @IsIn(['asc', 'desc'])
    order?: 'asc' | 'desc' = 'asc';
}