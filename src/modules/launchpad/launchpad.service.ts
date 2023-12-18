import {
  Injectable,
  NotFoundException,
  HttpException,
  HttpStatus,
} from '@nestjs/common';
import { CreateLaunchpadDto } from './dto/create-launchpad.dto';
import { UpdateLaunchpadDto } from './dto/update-launchpad.dto';
import { PrismaService } from 'src/prisma/prisma.service';
import { ProjectEntity } from './entities/project.entity';
import { RoundEntity } from './entities/round.entity';
import { GraphQLClient, gql } from 'graphql-request';
import { CheckStakingDto } from './dto/check-staking.dto';
import { Prisma, User } from '@prisma/client';
import { validate as isValidUUID } from 'uuid';
import { FindAllProjectDto } from './dto/find-all-project.dto';
import { Redis } from 'src/database';
import { SubcribeProjectDto } from './dto/subcribe-project.dto';
import { SubcribeEntity } from './entities/subcribe.entity';
import { ProjectStat } from 'src/constants/enums/ProjectStat.enum';

@Injectable()
export class LaunchpadService {
  private readonly endpoint = process.env.SUBGRAPH_URL_STAKING;

  private client = this.getGraphqlClient();

  private getGraphqlClient() {
    return new GraphQLClient(this.endpoint);
  }

  constructor(private readonly prisma: PrismaService) {}
  // create(createLaunchpadDto: CreateLaunchpadDto) {
  //   return 'This action adds a new launchpad';
  // }

  async configNextRound(id: string) {
    await Redis.publish('project-channel', {
      data: {
        id,
      },
      process: 'config-round-timer',
    });
  }

  async findAll(query: FindAllProjectDto): Promise<any> {
    const projects = await this.prisma.project.findMany({
      where: {
        isActivated: true,
      },
      include: {
        collection: true,
        rounds: {
          include: {
            round: true,
          },
        },
      },
    });
    let filteredProjects = [];
    if (query.mode === ProjectStat.UPCOMING) {
      filteredProjects = projects.filter((project) =>
        project.rounds.every((round) => {
          console.log(project.id, new Date(round.start) > new Date());
          return new Date(round.start) > new Date();
        }),
      );
    } else if (query.mode === ProjectStat.MINTING) {
      filteredProjects = projects.filter((project) =>
        project.rounds.some((round) => {
          console.log(
            new Date(round.start) <= new Date() &&
              new Date(round.end) >= new Date(),
          );
          return (
            new Date(round.start) <= new Date() &&
            new Date(round.end) >= new Date()
          );
        }),
      );
    } else if (query.mode === ProjectStat.ENDED) {
      filteredProjects = projects.filter((project) =>
        project.rounds.every((round) => new Date(round.end) < new Date()),
      );
    } else {
      filteredProjects = projects;
    }
    const returnProject = filteredProjects.map((item) => {
      return {
        ...item,
        rounds: item.rounds.map((round) => ({
          ...round,
          ...round.round,
        })),
      };
    });
    // console.log(returnProject[0].rounds)
    return returnProject;
  }

  async findOne(id: string): Promise<ProjectEntity> {
    const project = await this.prisma.project.findUnique({
      where: {
        id,
      },
      include: {
        collection: true,
        rounds: {
          include: {
            round: true,
          },
        },
      },
    });
    if (!project) throw new NotFoundException('Project not found');
    return {
      ...project,
      rounds: project.rounds.map((round) => ({
        ...round,
        ...round.round,
      })),
    };
  }

  async checkStaking(input: CheckStakingDto, user: User) {
    try {
      const { projectId } = input;
      if (!projectId) {
        throw Error('Please Enter Project');
      }
      if (!isValidUUID(projectId)) {
        throw new Error('Invalid Project. Please try again !');
      }
      const result = await this.prisma.project.findFirst({
        where: {
          id: projectId,
        },
        include: {
          subscriber: {
            select: {
              subscribeDate: true,
              user: {
                select: {
                  id: true,
                  email: true,
                  avatar: true,
                  username: true,
                  publicKey: true,
                },
              },
            },
          },
        },
      });

      if (!result) {
        throw new NotFoundException();
      }
      const { subscriber } = result;
      const query = gql`
        query getStaking($id: ID) {
          delegator(id: $id) {
            totalLockStake
            totalClaimedRewards
            stakedAmount
            id
            createdOn
            address
          }
        }
      `;

      const listStaking = await Promise.all(
        subscriber.map(async (item) => {
          const { user } = item;
          const response = await this.client.request(query, {
            id: user.publicKey.toLowerCase(),
          });
          const { delegator }: any = response;
          return { ...item, ...delegator };
        }),
      );

      for (const item of listStaking) {
        const { user } = item;
        await this.prisma.userProject.updateMany({
          where: {
            userId: user.id,
            projectId: projectId,
          },
          data: {
            stakingTotal: item.stakedAmount,
            lastDateRecord: new Date(),
          },
        });
      }

      return result;
    } catch (error) {
      console.log(error);
      throw new HttpException(`${error.message}`, HttpStatus.BAD_REQUEST);
    }
  }

  async subcribeProject(
    input: SubcribeProjectDto,
    user: User,
  ): Promise<SubcribeEntity> {
    try {
      if (!isValidUUID(input.projectId)) {
        throw new Error('Invalid Project. Please try again !');
      }
      const projectExists = await this.prisma.project.findUnique({
        where: {
          id: input.projectId,
        },
      });
      if (!projectExists) {
        throw new NotFoundException();
      }

      const isSubcribe = await this.prisma.userProject.findFirst({
        where: {
          userId: user.id,
          projectId: input.projectId,
        },
      });

      if (isSubcribe) {
        throw Error('You are a subscriber to this project');
      }
      const response = await this.prisma.userProject.create({
        data: {
          userId: user.id,
          projectId: input.projectId,
        },
      });
      return response;
    } catch (error) {
      throw new HttpException(`${error.message}`, HttpStatus.BAD_REQUEST);
    }
  }
}
