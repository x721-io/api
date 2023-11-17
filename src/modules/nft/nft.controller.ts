import { Controller, Get, Post, Body, Patch, Param, Delete, Query , UseGuards } from '@nestjs/common';
import { GetCurrentUser } from 'src/decorators/get-current-user.decorator';
import { User } from '@prisma/client';
import { CreateNftDto } from './dto/create-nft.dto';
import { UpdateNftDto } from './dto/update-nft.dto';
import { NftService } from './nft.service';
import { AuthenticationGuard } from '../auth/guards/auth.guard';

@Controller('nft')
export class NftController {
  constructor(private readonly nftService : NftService){}

  @Post()
  @UseGuards(AuthenticationGuard)
  create(@Body() createNftDto : CreateNftDto ,  @GetCurrentUser() user: User ) {
    return this.nftService.create(createNftDto , user);
  }

  @Get()
  findall(){
    return this.nftService.findAll();
  }

  @Get(':id')
  findOne(@Param('id') id : string ){
    return this.nftService.findOne(id);
  }

  @Get('/user/:id')
  findNFTByUserID(@Param('id') id : string){
    return this.nftService.findNFTByUserID(id);
  }

}
