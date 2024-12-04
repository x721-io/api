import {
  BadRequestException,
  ForbiddenException,
  HttpException,
  HttpStatus,
  Injectable,
  NotFoundException,
} from '@nestjs/common';
import {
  CreatePlatformOverviewDto,
  PlatformOverviewFilter,
} from './dto/create-platform-overview.dto';
import { UpdatePlatformOverviewDto } from './dto/update-platform-overview.dto';
import { PrismaService } from '../../prisma/prisma.service';
import { logger } from '../../commons';
import { User } from '@prisma/client';
import PaginationCommon from '../../commons/HasNext.common';

@Injectable()
export class PlatformOverviewService {
  constructor(private readonly prisma: PrismaService) {}

  async create(
    createPlatformOverviewDto: CreatePlatformOverviewDto,
    user: User,
  ) {
    try {
      return await this.prisma.platform.create({
        data: {
          platform: createPlatformOverviewDto.platform,
          creator: user.id,
          name: createPlatformOverviewDto.name,
          avatar: createPlatformOverviewDto.avatar,
          banner: createPlatformOverviewDto.banner,
          description: createPlatformOverviewDto.description,
        },
      });
    } catch (error) {
      logger.error(error);
      throw new HttpException(
        `${error.message}`,
        HttpStatus.SERVICE_UNAVAILABLE,
      );
    }
  }

  async findAll(
    filter: PlatformOverviewFilter,
  ): Promise<PagingResponseHasNext<any>> {
    try {
      let whereQuery: any = {};
      if (filter.platform && filter.platform.length > 0) {
        whereQuery = {
          platform: filter.platform,
        };
      }

      let templateQuery: boolean | any = true;
      if (filter.templateStatus && filter.templateStatus.length > 0) {
        templateQuery = {
          where: {
            isActive:
              filter.templateStatus === 'true' || filter.templateStatus === 't',
          },
        };
      }

      const listPlatform = await this.prisma.platform.findMany({
        skip: (filter.page - 1) * filter.limit,
        take: filter.limit,
        where: whereQuery,
        include: {
          templates: {
            orderBy: {
              createdAt: 'asc',
            },
            ...templateQuery,
          },
        },
        orderBy: {
          createdAt: 'desc',
        },
      });

      const hasNext = await PaginationCommon.hasNextPage(
        filter.page,
        filter.limit,
        'platform',
        whereQuery,
      );

      return {
        data: listPlatform,
        paging: {
          limit: filter.limit,
          page: filter.page,
          hasNext: hasNext,
        },
      };
    } catch (error) {
      logger.error(error);
      throw new HttpException(
        `${error.message}`,
        HttpStatus.SERVICE_UNAVAILABLE,
      );
    }
  }

  async findOne(id: string, filter: PlatformOverviewFilter) {
    let whereQuery: any = {};
    if (filter.platform && filter.platform.length > 0) {
      whereQuery = {
        platform: filter.platform,
      };
    }

    let templateQuery: boolean | any = true;
    if (filter.templateStatus && filter.templateStatus.length > 0) {
      templateQuery = {
        where: {
          isActive:
            filter.templateStatus === 'true' || filter.templateStatus === 't',
        },
      };
    }

    return this.prisma.platform.findFirst({
      where: {
        id: id,
        ...whereQuery,
      },
      include: {
        templates: {
          orderBy: {
            createdAt: 'asc',
          },
          ...templateQuery,
        },
      },
    });
  }

  async update(
    id: string,
    updatePlatformOverviewDto: UpdatePlatformOverviewDto,
    user: User,
  ) {
    try {
      const platform = await this.prisma.platform.findFirst({
        where: {
          id: id,
        },
      });

      if (!platform) {
        throw new NotFoundException();
      }
      if (platform.creator != user.id) {
        throw new ForbiddenException();
      }

      await this.prisma.platform.update({
        where: {
          id: id,
        },
        data: {
          name: updatePlatformOverviewDto.name,
          platform: updatePlatformOverviewDto.platform,
          avatar: updatePlatformOverviewDto.avatar,
          banner: updatePlatformOverviewDto.banner,
          description: updatePlatformOverviewDto.description,
        },
      });
    } catch (error) {
      logger.error(error);
      throw new HttpException(
        `${error.message}`,
        HttpStatus.SERVICE_UNAVAILABLE,
      );
    }
  }

  async remove(id: string, user: User) {
    try {
      const platform = await this.prisma.platform.findFirst({
        where: {
          id: id,
        },
      });

      if (!platform) {
        throw new NotFoundException();
      }
      if (platform.creator != user.id) {
        throw new ForbiddenException();
      }

      await this.prisma.overviewTemplate.deleteMany({
        where: {
          platformId: id,
        },
      });

      await this.prisma.overviewTemplate.delete({
        where: {
          id: id,
        },
      });

      return 'ok';
    } catch (error) {
      logger.error(error);
      throw new HttpException(
        `${error.message}`,
        HttpStatus.SERVICE_UNAVAILABLE,
      );
    }
  }
}
