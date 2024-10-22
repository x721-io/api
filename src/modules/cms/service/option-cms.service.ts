import { Injectable, HttpException, HttpStatus } from '@nestjs/common';

import { PrismaService } from 'src/prisma/prisma.service';
import OtherCommon from 'src/commons/Other.common';
import { validate as isValidUUID } from 'uuid';
import { Prisma } from '@prisma/client';
import PaginationCommon from 'src/commons/HasNext.common';
import { OptionDto } from '../dto/marketplace.dto';
@Injectable()
export class CMSOptionService {
  constructor(private readonly prisma: PrismaService) {}

  async getCollectionOption(
    input: OptionDto,
  ): Promise<PagingResponseHasNext<any>> {
    try {
      const whereCondition: Prisma.CollectionWhereInput = {};
      whereCondition.AND = [{ isActive: true }];
      if (input.search) {
        if (!isValidUUID(input.search)) {
          whereCondition.AND.push({
            OR: [
              { shortUrl: input.search },
              { address: input.search },
              {
                nameSlug: {
                  contains: OtherCommon.stringToSlugSearch(input.search),
                  mode: 'insensitive',
                },
              },
            ],
          });
        } else {
          whereCondition.AND.push({
            OR: [{ id: input.search }],
          });
        }
      }
      if (input.type) {
        whereCondition.AND.push({
          type: input.type,
        });
      }
      const collections = await this.prisma.collection.findMany({
        where: whereCondition,
        skip: (input.page - 1) * input.limit,
        take: input.limit,
      });

      const listOptionCollection = collections.map((item) => {
        return {
          key: item.id,
          value: item.address,
          label: item.name,
          ...item,
        };
      });
      const hasNext = await PaginationCommon.hasNextPage(
        input.page,
        input.limit,
        'collection',
        whereCondition,
      );
      return {
        data: listOptionCollection,
        paging: {
          limit: input.limit,
          page: input.page,
          hasNext: hasNext,
        },
      };
    } catch (error) {
      throw new HttpException(`${error.message}`, HttpStatus.BAD_REQUEST);
    }
  }

  async getRoundOption(input: OptionDto): Promise<PagingResponseHasNext<any>> {
    try {
      const whereCondition: Prisma.RoundInfoWhereInput = {};
      whereCondition.AND = [{ isActive: true }, { isDelete: false }];
      if (input.search) {
        if (Number(input.search)) {
          whereCondition.AND.push({
            OR: [
              {
                name: {
                  contains: OtherCommon.stringToSlugSearch(input.search),
                  mode: 'insensitive',
                },
              },
              { id: Number(input.search) },
            ],
          });
        }
      }
      const listRoundInfo = await this.prisma.roundInfo.findMany({
        where: whereCondition,
        skip: (input.page - 1) * input.limit,
        take: input.limit,
      });

      const hasNext = await PaginationCommon.hasNextPage(
        input.page,
        input.limit,
        'roundInfo',
        whereCondition,
      );
      return {
        data: listRoundInfo,
        paging: {
          limit: input.limit,
          page: input.page,
          hasNext: hasNext,
        },
      };
    } catch (error) {
      throw new HttpException(`${error.message}`, HttpStatus.BAD_REQUEST);
    }
  }
}
