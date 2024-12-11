import { Injectable } from '@nestjs/common';
import { PrismaService } from '../../prisma/prisma.service';
import { Prisma } from '@prisma/client';
import { NftDto } from './dto/nft.dto';
import OtherCommon from 'src/commons/Other.common';

@Injectable()
export class LayerService {
  constructor(private prisma: PrismaService) {}

  async findNFTs(params: {
    page?: number;
    limit?: number;
    where?: {
      collection?: {
        metadata?: {
          name?: string;
          categoryId?: string;
          categoryName?: string;
        };
        name?: string;
        symbol?: string;
        address?: string;
      };
    };
    orderBy?: {
      createdAt?: 'asc' | 'desc';
    };
  }): Promise<PagingResponseHasNext<NftDto>> {
    const { page, limit, where, orderBy } = params;

    // Build the where clause for collection filtering
    const whereClause: Prisma.NFTWhereInput = {
      isActive: true,
      collection: {},
    };

    // Add collection filters if provided
    if (where?.collection) {
      const { metadata, name, symbol, address } = where.collection;

      // Handle metadata filters
      if (metadata) {
        if (metadata.name) {
          whereClause.collection.metadataJson = {
            path: ['name'],
            string_contains: metadata.name,
          };
        }

        if (metadata.categoryId) {
          whereClause.collection.metadataJson = {
            path: ['category'],
            array_contains: [{ id: metadata.categoryId }],
          };
        }

        if (metadata.categoryName) {
          whereClause.collection.metadataJson = {
            path: ['category'],
            array_contains: [{ name: metadata.categoryName }],
          };
        }
      }

      // Add basic collection filters
      if (name) {
        whereClause.collection.nameSlug = {
          contains: OtherCommon.stringToSlug(name),
          mode: 'insensitive',
        };
      }

      if (symbol) {
        whereClause.collection.symbol = {
          contains: symbol,
          mode: 'insensitive',
        };
      }

      if (address) {
        whereClause.collection.address = address;
      }
    }

    // Get total count for pagination
    const total = await this.prisma.nFT.count({
      where: whereClause,
    });

    // Get NFTs with collection data
    const nfts = await this.prisma.nFT.findMany({
      skip: (page - 1) * limit || 0,
      take: limit || 10,
      where: whereClause,
      orderBy: orderBy || { createdAt: 'desc' },
      include: {
        collection: {
          select: {
            id: true,
            name: true,
            symbol: true,
            address: true,
            metadataJson: true,
            isVerified: true,
            floorPrice: true,
            floor: true,
            avatar: true,
          },
        },
        traits: true,
      },
    });

    return {
      data: nfts,
      paging: {
        page: page || 0,
        limit: limit || 10,
        hasNext: (page || 0) + (limit || 10) < total,
      },
    };
  }
}
