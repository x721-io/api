import { PartialType } from '@nestjs/mapped-types';
import { CreateUserDto } from './create-user.dto';
import {NFTTab} from '../../../constants/enums/NFTTab.enum';
import { IsNotEmpty, IsString , IsOptional, IsEnum , IsNumber , IsNumberString, Min, IsBoolean , IsDefined } from 'class-validator';
import { Transform } from 'class-transformer';
import { Optional } from '@nestjs/common';
import {NFTIsBid} from '../../../constants/enums/NFTTab.enum';


export class FilterNFTUserDetail  {
    @IsEnum(NFTTab)
    tab: NFTTab;

    @IsNumber()
    @Transform(({ value }) => {
        return Number(value);
    })
    @Min(0)
    @IsNotEmpty()
    page : number

    @IsNumber()

    @Transform(({ value }) => {
        return Number(value);
    })
    @IsNotEmpty()
    limit : number

    @IsOptional()
    @IsEnum(NFTIsBid)
    has_bid : NFTIsBid
}
