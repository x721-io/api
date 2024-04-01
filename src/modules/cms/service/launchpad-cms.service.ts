import {
  Injectable,
  HttpException,
  HttpStatus,
  NotFoundException,
} from '@nestjs/common';

import { PrismaService } from 'src/prisma/prisma.service';
import OtherCommon from 'src/commons/Other.common';
import { validate as isValidUUID } from 'uuid';
import { Account, Prisma } from '@prisma/client';
import PaginationCommon from 'src/commons/HasNext.common';
import {
  CreateOrUpdateProjectDto,
  CreateRoundInforDto,
  GetAllRoundDto,
  UpdateRoundInforDto,
} from '../dto/launchpad.dto';
import { CMSService } from './cms.service';
import { FindAllProjectDto } from '../dto/launchpad.dto';
import { ProjectEntity } from '../../launchpad/entities/project.entity';
import { ProjectStat } from 'src/constants/enums/ProjectStat.enum';

@Injectable()
export class LaunchPadService {
  constructor(
    private readonly prisma: PrismaService,
    private readonly cmsService: CMSService,
  ) {}

  async findAllRound(
    filter: GetAllRoundDto,
  ): Promise<PagingResponseHasNext<any>> {
    try {
      const whereCondition: Prisma.RoundInfoWhereInput = {
        AND: [{ name: filter.name }, { isActive: true }, { isDelete: false }],
      };
      const result = await this.prisma.roundInfo.findMany({
        where: whereCondition,
      });

      const hasNext = await PaginationCommon.hasNextPage(
        filter.page,
        filter.limit,
        'roundInfo',
        whereCondition,
      );
      return {
        data: result,
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

  async createRoundInfor(input: CreateRoundInforDto, account: Account) {
    try {
      const checkExist = await this.prisma.roundInfo.findFirst({
        where: {
          AND: [{ name: input.name }, { type: input.type }],
        },
      });
      if (checkExist) {
        throw new Error('Name round is already exists !');
      }
      await this.cmsService.handleActionLog(
        account.id,
        ` Account: ${account.id} Create Round Infomation ${input.name} with type ${input.type}`,
      );
      const count = await this.prisma.roundInfo.count();
      return await this.prisma.roundInfo.create({
        data: {
          id: Number(count + 1),
          name: input.name,
          type: input.type,
          description: input.description,
        },
      });
    } catch (error) {
      console.log(error);
      throw new HttpException(`${error.message}`, HttpStatus.BAD_REQUEST);
    }
  }

  async updateRoundInfor(input: UpdateRoundInforDto, account: Account) {
    try {
      if (!input.id) {
        throw new Error('Invalid Round. Please try again !');
      }
      const checkExist = await this.prisma.roundInfo.findUnique({
        where: {
          id: Number(input.id),
        },
      });

      if (!checkExist) {
        throw new NotFoundException();
      }
      const checkExistName = await this.prisma.roundInfo.findFirst({
        where: {
          AND: [
            { name: input.name },
            { type: input.type },
            { NOT: { id: Number(input.id) } },
          ],
        },
      });
      if (checkExistName) {
        throw new Error('Name round is already exists !');
      }
      await this.cmsService.handleActionLog(
        account.id,
        ` Account: ${account.id} Update Round Infomation ${input.name} with type ${input.type}`,
      );

      return await this.prisma.roundInfo.update({
        data: {
          name: input.name,
          type: input.type,
          description: input.description,
        },
        where: {
          id: Number(input.id),
        },
      });
    } catch (error) {
      console.log(error);
      throw new HttpException(`${error.message}`, HttpStatus.BAD_REQUEST);
    }
  }

  async deleteRound(id: string, account: Account) {
    try {
      if (!id) {
        throw new Error('Invalid Round. Please try again !');
      }
      const checkExist = await this.prisma.roundInfo.findUnique({
        where: {
          id: Number(id),
        },
      });
      if (!checkExist) {
        throw new NotFoundException();
      }
      await this.cmsService.handleActionLog(
        account.id,
        ` Account: ${account.id} Delete Round Info: ${checkExist.name} - ${checkExist.type}`,
      );
      return await this.prisma.roundInfo.update({
        where: {
          id: Number(id),
        },
        data: {
          isDelete: true,
          isActive: false,
        },
      });
    } catch (error) {
      throw new HttpException(`${error.message}`, HttpStatus.BAD_REQUEST);
    }
  }

  async findAllProject(
    query: FindAllProjectDto,
  ): Promise<PagingResponseHasNext<ProjectEntity>> {
    try {
      const whereCondition: Prisma.ProjectWhereInput = {
        isActivated: true,
        name: query.name,
      };
      const projects = await this.prisma.project.findMany({
        where: {
          AND: [{ isActivated: true }, { isDelete: false }],
        },
        include: {
          collection: true,
          rounds: {
            include: {
              round: {
                include: {
                  rangeTime: true,
                },
              },
            },
            orderBy: {
              start: 'asc',
            },
          },
        },
      });
      let filteredProjects = [];
      if (query.mode === ProjectStat.UPCOMING) {
        filteredProjects = projects.filter((project) =>
          project.rounds.every((round) => {
            return new Date(round.start) > new Date();
          }),
        );
      } else if (query.mode === ProjectStat.MINTING) {
        filteredProjects = projects.filter((project) =>
          project.rounds.some((round) => {
            return (
              (new Date(round.start) <= new Date() &&
                new Date(round.end) >= new Date()) ||
              new Date(round.claimableStart) === new Date(0)
            );
          }),
        );
      } else if (query.mode === ProjectStat.ENDED) {
        filteredProjects = projects.filter((project) =>
          project.rounds.every((round) => new Date(round.end) < new Date()),
        );
      } else if (query.mode === ProjectStat.CLAIM) {
        filteredProjects = projects.filter((project) =>
          project.rounds.some(
            (round) => new Date(round.claimableStart) < new Date(),
          ),
        );
      } else {
        filteredProjects = projects;
      }

      const returnProject = filteredProjects.map((item) => ({
        ...item,
        rounds: (item.rounds || []).map((roundItem) => {
          const round = roundItem?.round || {};
          const rangeTime = round?.rangeTime || [];
          const formattedRanges = this.formatRanges(rangeTime);
          return {
            ...roundItem,
            ...roundItem.round,
            round: {
              ...roundItem.round,
              rangeTime: formattedRanges,
            },
            rangeTime: formattedRanges,
          };
        }),
      }));

      const hasNext = await PaginationCommon.hasNextPage(
        query.page,
        query.limit,
        'project',
        whereCondition,
      );
      return {
        data: returnProject,
        paging: {
          hasNext: hasNext,
          limit: query.limit,
          page: query.page,
        },
      };
    } catch (error) {
      throw new HttpException(`${error.message}`, HttpStatus.BAD_REQUEST);
    }
  }

  private formatRanges(rangeTime: any[]): any[] {
    return rangeTime.map((range) => ({
      ...range,
      start: OtherCommon.convertToTimeFormat(range.start),
      end: OtherCommon.convertToTimeFormat(range.end),
    }));
  }

  async handleCreateProject(input: CreateOrUpdateProjectDto, account: Account) {
    try {
      console.log(input);
      const checkExist = await this.prisma.project.findFirst({
        where: {
          name: input.name,
          AND: [
            { name: input.name },
            { isActivated: true },
            { isDelete: false },
          ],
        },
      });
      if (checkExist) {
        throw new Error('Name project is already exists!');
      }
      const checkCollection = await this.prisma.collection.findUnique({
        where: {
          address: input.collectionAddress.toLocaleLowerCase(),
        },
      });
      if (!checkCollection) {
        throw new NotFoundException();
      }
      const newProject = await this.prisma.project.create({
        data: {
          name: input.name,
          idOnchain: Number(input.idOnchain),
          banner: input.banner,
          description: input.description,
          organization: input.organization,
          website: input.website,
          details: input.details,
          twitter: input.twitter,
          telegram: input.telegram,
          discord: input.discord,
          facebook: input.facebook,
          instagram: input.instagram,
          logo: input.logo,
          isActivated: input.isActivated,
        },
      });
      if (!newProject) {
        throw new Error('Initialization failed');
      }
      await this.prisma.collection.update({
        data: {
          projectId: newProject.id,
        },
        where: {
          id: checkCollection.id,
        },
      });
      if (input.rounds && input.rounds.length > 0) {
        for (const round of input.rounds) {
          await this.prisma.projectRound.create({
            data: {
              projectId: newProject.id,
              roundId: Number(round.roundId),
              address: round.address,
              start: round.start,
              end: round.end,
              stakeBefore: round.stakeBefore,
              maxPerWallet: Number(round.maxPerWallet || 0),
              totalNftt: Number(round.totalNftt || 0),
              price: round.price || '0',
              instruction: round.instruction,
              requiredStaking: round.requiredStaking || '0',
              claimableStart: round.claimableStart,
              claimableIds: round.claimableIds,
            },
          });
        }
      }
      await this.cmsService.handleActionLog(
        account.id,
        ` Account: ${account.id} Create Project: ${newProject.name}}`,
      );
      return newProject;
    } catch (error) {
      console.log(error);
      throw new HttpException(`${error.message}`, HttpStatus.BAD_REQUEST);
    }
  }

  async handleUpdateProject(input: CreateOrUpdateProjectDto, account: Account) {
    try {
      if (!isValidUUID(input.id)) {
        throw new Error('Invalid Project. Please try again !');
      }

      const checkExistsProject = await this.prisma.project.findFirst({
        where: {
          AND: [{ id: input.id }, { isActivated: true }, { isDelete: false }],
        },
        include: {
          collection: true,
          rounds: {
            include: {
              round: {
                include: {
                  rangeTime: true,
                },
              },
            },
          },
        },
      });

      const checkExistCollection = await this.prisma.collection.findUnique({
        where: {
          address: input.collectionAddress.toLocaleLowerCase(),
        },
      });

      if (!checkExistsProject || !checkExistCollection) {
        throw new NotFoundException();
      }

      const checkDuplicateName = await this.prisma.project.findFirst({
        where: {
          AND: [
            { name: input.name },
            { NOT: { id: input.id } },
            { isActivated: true },
            { isDelete: false },
          ],
        },
      });

      if (checkDuplicateName) {
        throw new Error('Name Project is already exists !');
      }
      const updateProject = await this.prisma.project.update({
        data: {
          name: input.name,
          banner: input.banner,
          description: input.description,
          organization: input.organization,
          website: input.website,
          details: input.details,
          twitter: input.twitter,
          telegram: input.telegram,
          discord: input.discord,
          facebook: input.facebook,
          instagram: input.instagram,
          logo: input.logo,
          isActivated: input.isActivated,
        },
        where: {
          id: input.id,
        },
      });
      if (!updateProject) {
        throw new Error('Initialization failed');
      }

      if (checkExistsProject && checkExistsProject.collection) {
        await this.prisma.collection.update({
          where: {
            projectId: input.id,
          },
          data: {
            projectId: null,
          },
        });
      }
      await this.prisma.projectRound.deleteMany({
        where: {
          projectId: input.id,
        },
      });

      await this.prisma.collection.update({
        data: {
          projectId: input.id,
        },
        where: {
          id: checkExistCollection.id,
        },
      });
      if (input.rounds && input.rounds.length > 0) {
        for (const round of input.rounds) {
          await this.prisma.projectRound.create({
            data: {
              projectId: updateProject.id,
              roundId: Number(round.roundId),
              address: round.address,
              start: round.start,
              end: round.end,
              stakeBefore: round.stakeBefore,
              maxPerWallet: Number(round.maxPerWallet || 0),
              totalNftt: Number(round.totalNftt || 0),
              price: round.price || '0',
              instruction: round.instruction,
              requiredStaking: round.requiredStaking || '0',
              claimableStart: round.claimableStart,
              claimableIds: round.claimableIds,
            },
          });
        }
      }
      await this.cmsService.handleActionLog(
        account.id,
        ` Account: ${account.id} Update Project: ${updateProject.name}}`,
      );
      return updateProject;
    } catch (error) {
      console.log(error);
      throw new HttpException(`${error.message}`, HttpStatus.BAD_REQUEST);
    }
  }

  async handleDeleteProject(id: string, account: Account) {
    try {
      if (!id || !isValidUUID(id)) {
        throw new Error('Invalid Project. Please try again !');
      }
      const checkExist = await this.prisma.project.findFirst({
        where: {
          AND: [{ id: id }, { isActivated: true }, { isDelete: false }],
        },
        include: {
          collection: true,
          rounds: {
            include: {
              round: {
                include: {
                  rangeTime: true,
                },
              },
            },
          },
        },
      });
      if (!checkExist) {
        throw new NotFoundException();
      }
      if (checkExist && checkExist.collection) {
        await this.prisma.collection.update({
          where: {
            projectId: id,
          },
          data: {
            projectId: null,
          },
        });
      }
      await this.cmsService.handleActionLog(
        account.id,
        ` Account: ${account.id} Delete Project Info: ${checkExist.name}}`,
      );
      return await this.prisma.project.update({
        where: {
          id: id,
        },
        data: {
          isDelete: true,
          isActivated: false,
        },
      });
    } catch (error) {
      throw new HttpException(`${error.message}`, HttpStatus.BAD_REQUEST);
    }
  }
  async getDetailProject(id: string): Promise<ProjectEntity> {
    const project = await this.prisma.project.findFirst({
      where: {
        AND: [{ id }, { isActivated: true }, { isDelete: false }],
      },
      include: {
        collection: true,
        rounds: {
          include: {
            round: {
              include: {
                rangeTime: true,
              },
            },
          },
          orderBy: {
            start: 'asc',
          },
        },
      },
    });

    if (!project) {
      throw new NotFoundException();
    }

    const returnProject = {
      ...project,
      rounds: (project.rounds || []).map((roundItem) => {
        const { round } = roundItem;
        const rangeTime = round?.rangeTime || [];
        const formattedRanges = this.formatRanges(rangeTime);

        return {
          ...roundItem,
          ...roundItem.round,
          round: {
            ...roundItem.round,
            rangeTime: formattedRanges,
          },
          rangeTime: formattedRanges,
        };
      }),
    };

    return returnProject;
  }
}
