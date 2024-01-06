import {
  Injectable,
  NotFoundException,
  HttpException,
  HttpStatus,
} from '@nestjs/common';
import { PrismaService } from 'src/prisma/prisma.service';
import { ProjectEntity } from './entities/project.entity';
import { GraphQLClient, gql } from 'graphql-request';
import { CheckStakingDto } from './dto/check-staking.dto';
import { User } from '@prisma/client';
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
          orderBy: {
            start: 'asc',
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

  async checkStaking(input: CheckStakingDto) {
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
                  signer: true,
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
            id: user.signer.toLowerCase(),
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

  async subcribeProject(input: SubcribeProjectDto): Promise<SubcribeEntity> {
    try {
      if (!isValidUUID(input.projectId)) {
        throw new Error('Invalid Project. Please try again !');
      }
      const projectExists = await this.prisma.project.findUnique({
        where: {
          id: input.projectId,
        },
      });

      const user = await this.prisma.user.findFirst({
        where: {
          signer: input.walletAddress.toLowerCase(),
        },
      });
      if (!projectExists) {
        throw new NotFoundException('Project not found');
      }

      if (!user) {
        const newUser = await this.prisma.user.create({
          data: {
            signer: input.walletAddress.toLowerCase(),
          },
        });
        const response = await this.prisma.userProject.create({
          data: {
            userId: newUser.id,
            projectId: input.projectId,
          },
        });
        return response;
      } else {
        const subscriber = await this.prisma.user.findFirst({
          where: {
            signer: input.walletAddress.toLowerCase(),
          },
        });
        if (subscriber) {
          throw new Error('You have subscribed to the project');
        }
        const response = await this.prisma.userProject.create({
          data: {
            userId: user.id,
            projectId: input.projectId,
          },
        });
        return response;
      }
    } catch (error) {
      throw new HttpException(`${error.message}`, HttpStatus.BAD_REQUEST);
    }
  }

  async isSubscribed(
    walletAddress: string,
    projectId: string,
  ): Promise<boolean> {
    const subscriber = await this.prisma.user.findFirst({
      where: {
        signer: walletAddress.toLowerCase(),
      },
    });
    if (!subscriber) {
      return false;
    }
    const isSubcribe = await this.prisma.userProject.findUnique({
      where: {
        userId_projectId: {
          userId: subscriber.id,
          projectId,
        },
      },
    });
    return !!isSubcribe;
  }
}
