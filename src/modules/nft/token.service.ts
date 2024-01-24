// src/token/token.service.ts

import { Injectable, NotFoundException } from '@nestjs/common';
import { PrismaService } from 'src/prisma/prisma.service';
import { createHash } from 'crypto';
import { GraphQlcallerService } from '../graph-qlcaller/graph-qlcaller.service';
import { CONTRACT_TYPE } from '@prisma/client';
import { GetCollectionMarketData } from '../graph-qlcaller/getCollectionMarketData.service';

@Injectable()
export class TokenService {
  // private counter = 1n; // Initialize a counter
  constructor(
    private readonly prisma: PrismaService,
    private readonly graphql: GraphQlcallerService,
    private readonly tokenCountService: GetCollectionMarketData,
  ) {}

  async generateTokenId(
    minterAddress: string,
    collectionAddress: string,
  ): Promise<any> {
    const isValidAddress = await this.prisma.collection.findFirst({
      where: {
        address: {
          contains: collectionAddress,
          mode: 'insensitive',
        },
      },
    });
    if (!isValidAddress) {
      throw new NotFoundException('Collection address not found');
    }
    // Remove '0x' prefix and convert the minter address to BigInt
    // const minterBigInt = BigInt("0xf24c359B22728Ce712b81ee018344B58CEb55d51");

    // Shift the minter address 96 bits to the left
    // let tokenIdBigInt = minterBigInt << 96n;

    // Extract a portion of the collection address and current timestamp for uniqueness
    // Ensure that they together fit within 96 bits
    // const collectionPart = collectionAddress.slice(-12); // Last 12 characters (48 bits)
    // const timestamp = Date.now(); // Milliseconds timestamp (usually less than 48 bits)
    // const additionalData = this.prisma.nFT.count();

    // Combine the shifted minter address with the additional data
    // tokenIdBigInt |= additionalData;

    // Return the tokenId
    // return tokenIdBigInt.toString();

    // const current = await this.prisma.nFT.count({
    //   where: {
    //     collection: {
    //       address: collectionAddress,
    //     },
    //   },
    // });
    let total;
    if (isValidAddress.type === CONTRACT_TYPE.ERC1155) {
      const nftsFromSubgraph =
        await this.tokenCountService.getCollectionCount(collectionAddress);
      total = nftsFromSubgraph.erc1155Contract.count;
    } else {
      const nftsFromSubgraph =
        await this.tokenCountService.getCollectionCount(collectionAddress);
      total = nftsFromSubgraph.erc721Contract.count;
    }
    // const baseId = collectionAddress + current.toString();
    // const hash = createHash('sha256')
    //   .update(baseId)
    //   .digest('hex')
    //   .substring(0, 24);

    const nextId = total + 1;
    // return minterAddress + nextId.toString().padStart(24, '0');
    return {
      u2uId: minterAddress + nextId.toString().padStart(24, '0'),
      id: nextId,
    };
  }
}
