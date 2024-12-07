import { Controller, Get, Query } from '@nestjs/common';
import { ApiTags, ApiOperation, ApiOkResponse } from '@nestjs/swagger';
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
  // @ApiOkResponse({
  //   description: 'Returns NFTs and pagination data',
  //   schema: {
  //     type: 'object',
  //     properties: {
  //       nfts: {
  //         type: 'array',
  //         items: {
  //           type: 'object',
  //           properties: {
  //             id: { type: 'string' },
  //             name: { type: 'string' },
  //             image: { type: 'string' },
  //             collection: {
  //               type: 'object',
  //               properties: {
  //                 id: { type: 'string' },
  //                 name: { type: 'string' },
  //                 symbol: { type: 'string' },
  //                 address: { type: 'string' },
  //                 metadataJson: { type: 'object' },
  //                 isVerified: { type: 'boolean' },
  //                 floorPrice: { type: 'string' },
  //                 floor: { type: 'number' },
  //               },
  //             },
  //             traits: {
  //               type: 'array',
  //               items: {
  //                 type: 'object',
  //                 properties: {
  //                   trait_type: { type: 'string' },
  //                   value: { type: 'string' },
  //                 },
  //               },
  //             },
  //           },
  //         },
  //       },
  //       pagination: {
  //         type: 'object',
  //         properties: {
  //           total: { type: 'number' },
  //           skip: { type: 'number' },
  //           take: { type: 'number' },
  //           hasMore: { type: 'boolean' },
  //         },
  //       },
  //     },
  //   },
  // })
  async findNFTs(@Query() findNftsDto: FindNftsDto) {
    const { page, limit, collection, orderBy } = findNftsDto;

    return this.layerService.findNFTs({
      page,
      limit,
      where: {
        collection,
      },
      orderBy: {
        createdAt: orderBy,
      },
    });
  }
}
