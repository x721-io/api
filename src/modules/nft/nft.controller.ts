import { Controller, Get, Post, Body, Patch, Param, Delete, Query , UseGuards } from '@nestjs/common';
import { GetCurrentUser } from 'src/decorators/get-current-user.decorator';
import { User } from '@prisma/client';
import { CreateNftDto } from './dto/create-nft.dto';
import { UpdateNftDto } from './dto/update-nft.dto';
import { NftService } from './nft.service';
import { AuthenticationGuard } from '../auth/guards/auth.guard';
import { GetTokenIdDto } from './dto/get-token-id.dto';
import { TokenService } from './token.service';
import { GetAllNftDto } from './dto/get-all-nft.dto';

@Controller('nft')
export class NftController {
  constructor(private readonly nftService : NftService, private readonly tokenService: TokenService){}

  @Post()
  @UseGuards(AuthenticationGuard)
  create(@Body() createNftDto : CreateNftDto , @GetCurrentUser() user: User ) {
    return this.nftService.create(createNftDto , user);
  }

  @Get('/tokenId')
  @UseGuards(AuthenticationGuard)
  getTokenId(@Query() input: GetTokenIdDto, @GetCurrentUser() user: User) {
    return this.tokenService.generateTokenId(user.publicKey, input.collectionAddress);
  }

  @Get()
  findall(@Query() query: GetAllNftDto){
    return this.nftService.findAll(query);
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
