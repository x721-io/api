import {
  Injectable,
  HttpException,
  HttpStatus,
  NotFoundException,
} from '@nestjs/common';
import { PrismaService } from 'src/prisma/prisma.service';
import { Account, Prisma } from '@prisma/client';
import PaginationCommon from 'src/commons/HasNext.common';
import { createOrUpdateTopicDto, getAllTopicDto } from '../dto/topic.dto';

import { accountListSelect } from '../../../commons/definitions/Constraint.Object';
import { CMSService } from './cms.service';

@Injectable()
export class TopicService {
  constructor(
    private readonly prisma: PrismaService,
    private readonly cmsService: CMSService,
  ) {}

  async getAllTopic(
    filter: getAllTopicDto,
  ): Promise<PagingResponseHasNext<any>> {
    try {
      const whereCondition: Prisma.TopicWhereInput = {};
      whereCondition.AND = [{ isDelete: false }];
      if (filter.search) {
        whereCondition.AND.push({ nameTopic: filter.search });
      }
      const listTopic = await this.prisma.topic.findMany({
        skip: (filter.page - 1) * filter.limit,
        take: filter.limit,
        where: whereCondition,
        orderBy: {
          createdAt: filter.order,
        },
      });
      const hasNext = await PaginationCommon.hasNextPage(
        filter.page,
        filter.limit,
        'topic',
        whereCondition,
      );
      return {
        data: listTopic,
        paging: {
          hasNext: hasNext,
          limit: filter.limit,
          page: filter.page,
        },
      };
    } catch (error) {
      throw new HttpException(`${error.message}`, HttpStatus.BAD_REQUEST);
    }
  }

  async createTopic(input: createOrUpdateTopicDto, account: Account) {
    try {
      const checkExists = await this.prisma.topic.findFirst({
        where: {
          nameTopic: input.nameTopic,
        },
      });
      if (checkExists) {
        throw new Error('Name Topic is already exists');
      }
      return await this.prisma.topic.create({
        data: {
          nameTopic: input.nameTopic,
          createdBy: account.id,
          isActive: true,
          isDelete: false,
        },
      });
    } catch (error) {
      throw new HttpException(`${error.message}`, HttpStatus.BAD_REQUEST);
    }
  }

  async UpdateTopic(input: createOrUpdateTopicDto, account: Account) {
    try {
      const checkExists = await this.prisma.topic.findFirst({
        where: {
          AND: [
            { nameTopic: input.nameTopic },
            { NOT: { id: Number(input.id) } },
            { isActive: true },
            { isDelete: true },
          ],
        },
      });
      if (checkExists) {
        throw new Error('Name Topic is already exists');
      }
      return await this.prisma.topic.update({
        data: {
          nameTopic: input.nameTopic,
          createdBy: account.id,
          isActive: false,
          isDelete: false,
        },
        where: {
          id: Number(input.id),
        },
      });
    } catch (error) {
      throw new HttpException(`${error.message}`, HttpStatus.BAD_REQUEST);
    }
  }

  async getDetailTopic(id: string) {
    try {
      if (isNaN(Number(id))) {
        throw new Error('ID must be a valid number');
      }
      const checkExists = await this.prisma.topic.findFirst({
        where: {
          AND: [{ isDelete: false }, { id: Number(id) }],
        },
        include: {
          account: {
            select: accountListSelect,
          },
        },
      });
      if (!checkExists) {
        throw new NotFoundException();
      }
      return checkExists;
    } catch (error) {
      throw new HttpException(`${error.message}`, HttpStatus.BAD_REQUEST);
    }
  }

  async deleteTopic(id: string, account: Account) {
    try {
      if (isNaN(Number(id))) {
        throw new Error('ID must be a valid number');
      }
      const checkExists = await this.prisma.topic.findFirst({
        where: {
          AND: [{ isDelete: false }, { id: Number(id) }],
        },
      });
      if (!checkExists) {
        throw new NotFoundException();
      }
      await this.cmsService.handleActionLog(
        account.id,
        ` Account: ${account.id} Remove Topic Infomation ${checkExists.nameTopic}`,
      );
      return await this.prisma.topic.update({
        data: {
          isDelete: true,
          isActive: false,
        },
        where: {
          id: Number(id),
        },
      });
    } catch (error) {
      throw new HttpException(`${error.message}`, HttpStatus.BAD_REQUEST);
    }
  }
}
