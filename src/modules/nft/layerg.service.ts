import { Injectable } from '@nestjs/common';
import { PrismaService } from '../../prisma/prisma.service';
import { ORDERSTATUS, ORDERTYPE, Prisma } from '@prisma/client';
import { NftDto } from './dto/nft.dto';
import OtherCommon from 'src/commons/Other.common';
import { SourceType } from 'src/constants/enums/Source.enum';
import { NFTHepler } from './helper/nft-helper.service';
import PaginationCommon from 'src/commons/HasNext.common';

import {
  CollectionSelect,
  creatorSelect,
  orderNFTSelect,
  orderSelect,
} from 'src/commons/definitions/Constraint.Object';

@Injectable()
export class LayerService {
  constructor(
    private prisma: PrismaService,
    private nftHepler: NFTHepler,
  ) {}

  async findNFTs(params: {
    page?: number;
    limit?: number;
    nftName?: string;
    priceMin?: number;
    priceMax?: number;
    orderStatus?: ORDERSTATUS;
    orderType?: ORDERTYPE;
    quoteToken?: string;
    orderBy?: string;
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
    order?: 'asc' | 'desc';
    // }): Promise<PagingResponseHasNext<NftDto>> {
  }): Promise<any> {
    const {
      page,
      limit,
      where,
      orderBy,
      nftName,
      priceMax,
      priceMin,
      orderStatus,
      orderType,
      quoteToken,
      order,
    } = params;
    // Build the where clause for collection filtering
    const whereClause: Prisma.NFTWhereInput = {
      isActive: true,
      collection: {
        source: SourceType.LAYERG,
      },
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
    if (nftName) {
      whereClause.nameSlug = {
        contains: OtherCommon.stringToSlug(nftName),
        mode: 'insensitive',
      };
    }
    if (quoteToken !== undefined) {
      whereClause.OrderByTokenId = { some: {} };
      whereClause.OrderByTokenId.some.quoteToken = quoteToken.toLowerCase();
    }

    const whereOrder: Prisma.OrderWhereInput =
      this.nftHepler.generateWhereOrder({
        priceMax: priceMax,
        priceMin: priceMin,
        quoteToken: quoteToken,
      } as any);

    if (orderBy === 'price') {
      whereOrder.nftById = whereClause;
      const { result, hasNext } = await this.nftHepler.getListNFTWithOrder(
        {
          priceMax: priceMax,
          priceMin: priceMin,
          name: nftName,
          quoteToken: quoteToken,
          orderStatus: orderStatus,
          orderType: orderType,
          page: page,
          limit: limit,
          order: order,
        } as any,
        whereOrder,
      );
      return {
        data: result,
        paging: {
          hasNext: hasNext,
          limit: limit,
          page: page,
        },
      };
    } else {
      const orderByProperties: Prisma.NFTOrderByWithRelationAndSearchRelevanceInput[] =
        [];
      if (orderBy == 'time') {
        orderByProperties.push({ createdAt: order });
      } else {
        orderByProperties.push({ metricPoint: 'desc' });
        orderByProperties.push({ createdAt: 'desc' });
      }

      if (
        priceMin !== undefined ||
        priceMax !== undefined ||
        orderStatus !== undefined ||
        orderType !== undefined
      ) {
        whereClause.OrderByTokenId = { some: {} };
        whereClause.OrderByTokenId.some.priceNum = {};
        if (priceMin !== undefined) {
          whereClause.OrderByTokenId.some.priceNum.gte = Number(
            OtherCommon.weiToEther(priceMin),
          );
        }
        if (priceMax !== undefined) {
          whereClause.OrderByTokenId.some.priceNum.lte = Number(
            OtherCommon.weiToEther(priceMax),
          );
        }
        whereClause.OrderByTokenId.some.orderStatus = orderStatus;
        whereClause.OrderByTokenId.some.orderType = { in: ['BULK', 'SINGLE'] };
        whereClause.OrderByTokenId.some.end = {
          gt: Math.floor(Date.now() / 1000),
        };

        whereClause.OrderByTokenId.some.quoteToken =
          (quoteToken ? quoteToken.toLowerCase() : process.env.NATIVE_U2U) ??
          process.env.NATIVE_U2U;
      }
      const nfts = await this.prisma.nFT.findMany({
        skip: (page - 1) * limit,
        take: limit,
        where: whereClause,
        orderBy: orderByProperties,
        include: {
          creator: {
            select: creatorSelect,
          },
          collection: {
            select: CollectionSelect,
          },
          OrderByTokenId: {
            select: orderNFTSelect,
            where: whereOrder,
            take: 1,
            orderBy: { priceNum: 'asc' },
          },
        },
      });
      const Nftformat = await this.nftHepler.handleFormatNFTResponseOrder(nfts);
      const hasNext = await PaginationCommon.hasNextPage(
        page,
        Math.floor(limit / 2),
        'nFT',
        whereClause,
      );
      return {
        data: Nftformat,
        paging: {
          hasNext: hasNext,
          limit: limit,
          page: page,
        },
      };
    }
  }
}
