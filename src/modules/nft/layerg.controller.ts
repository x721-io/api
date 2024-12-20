import { Controller, Get, Query } from '@nestjs/common';
import { ApiTags, ApiOperation } from '@nestjs/swagger';
import { LayerService } from './layerg.service';
import { FindNftsDto } from './dto/find-nfts.dto';

@ApiTags('NFTs')
@Controller('layerg')
export class LayerController {
  constructor(private readonly layerService: LayerService) {}

  @Get()
  @ApiOperation({
    summary: 'Find NFTs with filters',
    description: 'Retrieve NFTs with collection-based filtering and pagination',
  })
  async findNFTs(@Query() findNftsDto: FindNftsDto) {
    const { page, limit, collection, orderBy, nftName } = findNftsDto;

    return this.layerService.findNFTs({
      page,
      limit,
      nftName,
      where: {
        collection,
      },
      orderBy: {
        createdAt: orderBy,
      },
    });
  }
}
