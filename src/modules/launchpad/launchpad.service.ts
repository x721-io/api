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
    const whereRounds: Prisma.ProjectRoundWhereInput = {};

    if (query.start) {
      whereRounds.start = { gte: new Date(query.start) };
    }

    if (query.end) {
      whereRounds.end = { lte: new Date(query.end) };
    }
    const projects = await this.prisma.project.findMany({
      where: {
        isActivated: true,
      },
      include: {
        collection: true,
        rounds: {
          where: whereRounds,
          include: {
            round: true,
          },
        },
      },
    });
    // if (!projects)
    const returnProject = projects.map((item) => {
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
        throw Error('The Project does not exist');
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
}
