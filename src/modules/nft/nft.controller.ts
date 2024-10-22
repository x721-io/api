import {
  Controller,
  Get,
  Post,
  Body,
  Patch,
  Param,
  Delete,
  Query,
  UseGuards,
  ValidationPipe,
  UsePipes,
} from '@nestjs/common';
import { GetCurrentUser } from 'src/decorators/get-current-user.decorator';
import { User, Account } from '@prisma/client';
import { CreateNftDto } from './dto/create-nft.dto';
import { UpdateNftDto } from './dto/update-nft.dto';
import { NftService } from './nft.service';
import { AuthenticationGuard } from '../auth/guards/auth.guard';
import { GetTokenIdDto } from './dto/get-token-id.dto';
import { TokenService } from './token.service';
import { GetAllNftDto } from './dto/get-all-nft.dto';
import { MarketplaceService } from './nft-marketplace.service';
import { GetEventBase } from './dto/event-base.dto';
import { GetActivityBase } from './dto/activity-nft.dto';
import { GetGeneralInforDto } from './dto/get-general-infor.dto';
@Controller('nft')
export class NftController {
  constructor(
    private readonly nftService: NftService,
    private readonly tokenService: TokenService,
    private readonly eventService: MarketplaceService,
  ) {}

  @Get('crawl-nft-info')
  async crawlNftInfo(
    @Query('collectionAddress') address: string,
    @Query('txCreation') txCreation?: string,
  ) {
    return await this.nftService.crawlNftInfo(address, txCreation);
  }

  @Post()
  @UseGuards(AuthenticationGuard)
  create(@Body() createNftDto: CreateNftDto, @GetCurrentUser() user: User) {
    return this.nftService.create(createNftDto, user);
  }

  @Get('/tokenId')
  @UseGuards(AuthenticationGuard)
  getTokenId(@Query() input: GetTokenIdDto, @GetCurrentUser() user: User) {
    return this.tokenService.generateTokenId(
      user.publicKey,
      input.collectionAddress,
    );
  }

  @Post('/search')
  findall(@Body() query: GetAllNftDto) {
    return this.nftService.findAll(query);
  }

  @Get('/nftTransactionInfo')
  findNftsCurrentStatus(
    @Query('id') id: string,
    @Query('collectionAddress') collectionAddress: string,
    @Query('bidListPage') bidPage: number,
    @Query('bidListLimit') bidLimit: number,
  ) {
    return this.nftService.getNftDetailTransactionInfo(
      id,
      collectionAddress,
      bidPage,
      bidLimit,
    );
  }

  @Get('')
  findOne(
    @Query('id') id: string,
    @Query('collectionAddress') collectionAddress: string,
  ) {
    return this.nftService.findOne(id, collectionAddress);
  }

  @Get('/user/:id')
  findNFTByUserID(@Param('id') id: string) {
    return this.nftService.findNFTByUserID(id);
  }

  @Post('/events')
  @UsePipes(new ValidationPipe({ transform: true }))
  findEvents(@Body() input: GetEventBase) {
    return this.eventService.findEvents1(input);
  }

  @Post('/activity')
  findActivityNFT(@Body() input: GetActivityBase) {
    return this.nftService.findActivityNFT(input);
  }
  @Post('/general-count')
  getGeneralInfor(@Body() query: GetGeneralInforDto) {
    return this.nftService.getGeneralInfor(query);
  }
}
