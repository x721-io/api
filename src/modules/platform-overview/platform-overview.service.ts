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
import OtherCommon from 'src/commons/Other.common';

@Injectable()
export class PlatformOverviewService {
  constructor(private readonly prisma: PrismaService) {}

  async create(
    createPlatformOverviewDto: CreatePlatformOverviewDto,
    user: User,
  ) {
    try {
      const existPlatform = await this.prisma.platform.findFirst({
        where: {
          nameSlug: OtherCommon.stringToSlugSearch(
            createPlatformOverviewDto.nameSlug,
          ),
        },
      });
      if (!!existPlatform) {
        throw new BadRequestException('Slug name already exists');
      }

      return await this.prisma.platform.create({
        data: {
          platform: createPlatformOverviewDto.platform,
          nameSlug: OtherCommon.stringToSlugSearch(
            createPlatformOverviewDto.nameSlug,
          ),
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
      if (filter.templateStatus !== undefined) {
        templateQuery = {
          where: {
            isActive: filter.templateStatus,
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

  async findOne(nameSlug: string, filter: PlatformOverviewFilter) {
    let whereQuery: any = {};
    if (filter.platform && filter.platform.length > 0) {
      whereQuery = {
        platform: filter.platform,
      };
    }

    let templateQuery: boolean | any = true;
    if (filter.templateStatus !== null) {
      templateQuery = {
        where: {
          isActive: filter.templateStatus,
        },
      };
    }

    return this.prisma.platform.findFirst({
      where: {
        nameSlug: OtherCommon.stringToSlugSearch(nameSlug),
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
    nameSlug: string,
    updatePlatformOverviewDto: UpdatePlatformOverviewDto,
    user: User,
  ) {
    try {
      const platform = await this.prisma.platform.findFirst({
        where: {
          nameSlug: OtherCommon.stringToSlugSearch(nameSlug),
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
          nameSlug: OtherCommon.stringToSlugSearch(nameSlug),
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

  async remove(nameSlug: string, user: User) {
    try {
      const platform = await this.prisma.platform.findFirst({
        where: {
          nameSlug: OtherCommon.stringToSlugSearch(nameSlug),
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
          platformId: platform.id,
        },
      });

      await this.prisma.platform.delete({
        where: {
          id: platform.id,
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
