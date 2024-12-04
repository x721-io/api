import {
  BadRequestException,
  ForbiddenException,
  HttpException,
  HttpStatus,
  Injectable,
  NotFoundException,
} from '@nestjs/common';
import { CreatePlatformTemplateDto } from './dto/create-platform-template.dto';
import { UpdatePlatformTemplateDto } from './dto/update-platform-template.dto';
import { User } from '@prisma/client';
import { logger } from '../../commons';
import { PrismaService } from '../../prisma/prisma.service';
import { validate as isValidUUID } from 'uuid';
import { GetCurrentUser } from '../../decorators/get-current-user.decorator';

@Injectable()
export class PlatformTemplateService {
  constructor(private readonly prisma: PrismaService) {}

  async create(
    createPlatformTemplateDto: CreatePlatformTemplateDto,
    user: User,
  ) {
    try {
      if (!isValidUUID(createPlatformTemplateDto.platformId)) {
        throw new BadRequestException('Invalid UUID format');
      }
      const platform = await this.prisma.platform.findFirst({
        where: {
          id: createPlatformTemplateDto.platformId,
        },
      });
      if (!platform) {
        throw new NotFoundException();
      }

      return await this.prisma.overviewTemplate.create({
        data: {
          creator: user.id,
          name: createPlatformTemplateDto.name,
          avatar: createPlatformTemplateDto.avatar,
          banner: createPlatformTemplateDto.banner,
          description: createPlatformTemplateDto.description,
          sections: createPlatformTemplateDto.sections,
          isActive: createPlatformTemplateDto.isActive,
          Platform: {
            connect: {
              id: createPlatformTemplateDto.platformId,
            },
          },
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

  async findOne(id: string) {
    return this.prisma.overviewTemplate.findFirst({
      where: {
        id: id,
      },
    });
  }

  async update(
    id: string,
    updatePlatformTemplateDto: UpdatePlatformTemplateDto,
    user: User,
  ) {
    try {
      const template = await this.prisma.overviewTemplate.findFirst({
        where: {
          id: id,
        },
      });

      if (!template) {
        throw new NotFoundException();
      }
      if (template.creator != user.id) {
        throw new ForbiddenException();
      }

      await this.prisma.overviewTemplate.update({
        where: {
          id: id,
        },
        data: {
          name: updatePlatformTemplateDto.name,
          avatar: updatePlatformTemplateDto.avatar,
          banner: updatePlatformTemplateDto.banner,
          description: updatePlatformTemplateDto.description,
          sections: updatePlatformTemplateDto.sections,
          isActive: updatePlatformTemplateDto.isActive,
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
      const template = await this.prisma.overviewTemplate.findFirst({
        where: {
          id: id,
        },
      });

      if (!template) {
        throw new NotFoundException();
      }
      if (template.creator != user.id) {
        throw new ForbiddenException();
      }

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
