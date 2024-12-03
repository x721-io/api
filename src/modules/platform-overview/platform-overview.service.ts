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

  async findAll(filter: PlatformOverviewFilter) {
    try {
      const parsedLimit = parseInt(filter.limit || '20', 10); // Default limit is 20
      const parsedPage = parseInt(filter.page || '1', 10); // Default page is 1

      // Ensure valid numbers
      if (isNaN(parsedLimit) || isNaN(parsedPage)) {
        throw new Error('Invalid limit or page parameter');
      }

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

      return await this.prisma.platform.findMany({
        skip: (parsedPage - 1) * parsedLimit,
        take: parsedLimit,
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
