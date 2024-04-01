import {
  Injectable,
  HttpException,
  HttpStatus,
  NotFoundException,
} from '@nestjs/common';
import { PrismaService } from 'src/prisma/prisma.service';
import { validate as isValidUUID } from 'uuid';
import { Account, Prisma } from '@prisma/client';
import PaginationCommon from 'src/commons/HasNext.common';
import { CMSService } from './cms.service';
import { accountListSelect } from '../../../commons/definitions/Constraint.Object';

import {
  getAllBlog,
  createOrUpdateBlogDto,
  activeBlogDto,
} from '../dto/blog.dto';

@Injectable()
export class BlogService {
  constructor(
    private readonly prisma: PrismaService,
    private readonly cmsService: CMSService,
  ) {}

  async getAllBlog(filter: getAllBlog): Promise<PagingResponseHasNext<any>> {
    try {
      const whereCondition: Prisma.BlogWhereInput = {};
      whereCondition.AND = [];
      if (filter.topicId) {
        if (isNaN(Number(filter.topicId))) {
          throw new Error('Topic ID must be a valid number');
        }
        const checkExist = await this.prisma.topic.findUnique({
          where: {
            id: Number(filter.topicId),
          },
        });
        if (!checkExist) {
          throw new NotFoundException();
        }
        whereCondition.AND.push({
          topics: {
            some: {
              topic: {
                isActive: true,
                isDelete: false,
              },
              topicId: Number(filter.topicId),
            },
          },
        });
      }

      if (filter.search) {
        whereCondition.AND.push({
          OR: [
            {
              title: {
                contains: filter.search,
                mode: 'insensitive',
              },
            },
            isValidUUID(filter.search)
              ? {
                  id: filter.search,
                }
              : {},
          ],
        });
      }
      const listBlog = await this.prisma.blog.findMany({
        where: whereCondition,
        skip: (filter.page - 1) * filter.limit,
        take: filter.limit,
        include: {
          topics: {
            include: {
              topic: true,
            },
            where: {
              topic: {
                isActive: true,
                isDelete: false,
              },
            },
          },
        },
      });
      const hasNext = await PaginationCommon.hasNextPage(
        filter.page,
        filter.limit,
        'blog',
        whereCondition,
      );
      return {
        data: listBlog,
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

  async createBlog(input: createOrUpdateBlogDto, account: Account) {
    try {
      const checkExistName = await this.prisma.blog.findFirst({
        where: {
          AND: [{ title: input.title }, { isDelete: false }],
        },
      });
      if (checkExistName) {
        throw new Error('Title blog is already exists');
      }
      const newBlog = await this.prisma.blog.create({
        data: {
          title: input.title,
          description: input.description,
          createdBy: account.id,
          content: input.content,
          isActive: input.isActive,
        },
      });
      if (!newBlog) {
        throw new Error('Initialization failed');
      }
      if (input.topics && input.topics.length > 0) {
        for (const topic of input.topics) {
          const checkTopic = await this.prisma.topic.findUnique({
            where: {
              id: Number(topic.id),
            },
          });
          if (checkTopic) {
            await this.prisma.blogTopic.create({
              data: {
                topicId: Number(topic.id),
                blogId: newBlog.id,
              },
            });
          }
        }
      }
      await this.cmsService.handleActionLog(
        account.id,
        ` Account: ${account.id} Create Blog: ${newBlog.title}}`,
      );
      return newBlog;
    } catch (error) {
      throw new HttpException(`${error.message}`, HttpStatus.BAD_REQUEST);
    }
  }

  async updateBlog(input: createOrUpdateBlogDto, account: Account) {
    try {
      if (!isValidUUID(input.id) || !input.id) {
        throw new Error('Invalid Blog. Please try again !');
      }
      const existingBlog = await this.prisma.blog.findFirst({
        where: {
          AND: [
            { OR: [{ title: input.title }, { id: input.id }] },
            { isDelete: false },
          ],
        },
      });

      if (!existingBlog) {
        throw new NotFoundException();
      }

      if (existingBlog.id !== input.id) {
        throw new Error('Title blog is already exists');
      }

      const updateBlog = await this.prisma.blog.update({
        data: {
          title: input.title,
          description: input.description,
          content: input.content,
          isActive: input.isActive,
        },
        where: {
          id: input.id,
        },
      });
      if (!updateBlog) {
        throw new Error('Initialization failed');
      }
      await this.prisma.blogTopic.deleteMany({
        where: {
          blogId: updateBlog.id,
        },
      });
      if (input.topics && input.topics.length > 0) {
        for (const topic of input.topics) {
          const checkTopic = await this.prisma.topic.findUnique({
            where: {
              id: Number(topic.id),
            },
          });
          if (checkTopic) {
            await this.prisma.blogTopic.create({
              data: {
                topicId: Number(topic.id),
                blogId: updateBlog.id,
              },
            });
          }
        }
      }
      await this.cmsService.handleActionLog(
        account.id,
        ` Account: ${account.id} Update Blog: ${updateBlog.title}}`,
      );
      return updateBlog;
    } catch (error) {
      throw new HttpException(`${error.message}`, HttpStatus.BAD_REQUEST);
    }
  }

  async getDetail(id: string) {
    try {
      if (!isValidUUID(id) || !id) {
        throw new Error('Invalid Blog. Please try again !');
      }
      const blog = await this.prisma.blog.findFirst({
        where: {
          AND: [{ id: id }, { isDelete: false }],
        },
        include: {
          topics: {
            include: {
              topic: true,
            },
            where: {
              topic: {
                isActive: true,
                isDelete: false,
              },
            },
          },
          account: {
            select: accountListSelect,
          },
        },
      });
      if (!blog) {
        throw new NotFoundException();
      }
      return blog;
    } catch (error) {
      throw new HttpException(`${error.message}`, HttpStatus.BAD_REQUEST);
    }
  }

  async deleteBlog(id: string, account: Account) {
    try {
      if (!isValidUUID(id) || !id) {
        throw new Error('Invalid Blog. Please try again !');
      }
      const checkExists = await this.prisma.blog.findFirst({
        where: {
          AND: [{ isDelete: false }, { id: id }],
        },
      });
      if (!checkExists) {
        throw new NotFoundException();
      }

      await this.cmsService.handleActionLog(
        account.id,
        ` Account: ${account.id} Delete Blog: ${checkExists.title}}`,
      );
      return await this.prisma.blog.update({
        data: {
          isActive: false,
          isDelete: true,
        },
        where: {
          id: id,
        },
      });
    } catch (error) {
      throw new HttpException(`${error.message}`, HttpStatus.BAD_REQUEST);
    }
  }

  async activeBlog(input: activeBlogDto, account: Account) {
    try {
      if (!isValidUUID(input.id)) {
        throw new Error('Invalid Blog. Please try again !');
      }
      const checkExists = await this.prisma.blog.findFirst({
        where: {
          AND: [{ id: input.id }, { isDelete: false }],
        },
      });
      if (!checkExists) {
        throw new NotFoundException();
      }

      await this.cmsService.handleActionLog(
        account.id,
        ` Account: ${account.id} Delete Blog: ${checkExists.title}}`,
      );
      return await this.prisma.blog.update({
        data: {
          isActive: input.isActive,
        },
        where: {
          id: input.id,
        },
      });
    } catch (error) {
      throw new HttpException(`${error.message}`, HttpStatus.BAD_REQUEST);
    }
  }
}
