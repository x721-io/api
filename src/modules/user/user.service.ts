import {
  Injectable,
  NotFoundException,
  HttpException,
  HttpStatus,
} from '@nestjs/common';
import { UpdateUserDto } from './dto/update-user.dto';
import { PrismaService } from 'src/prisma/prisma.service';
import { Prisma, User } from '@prisma/client';
import { GetAllUser } from './dto/get-all-user.dto';
import { validate as isValidUUID } from 'uuid';
import { FindAllProjectDto } from '../launchpad/dto/find-all-project.dto';
import { findProjectsUserSubscribe } from '../launchpad/dto/find-project.dto';
import { ListProjectEntity } from './entities/project.entity';
import { UserEntity } from './entities/user.entity';
import { ActivityService } from '../nft/activity.service';
import { GetActivityBase } from './dto/activity-user.dto';
@Injectable()
export class UserService {
  constructor(
    private prisma: PrismaService,
    private activetiService: ActivityService,
  ) {}

  // Remove few prop secret
  private minifyUserObject(params: any): any {
    const { user = {} } = params;
    const propertiesToRemove = ['signature', 'signer', 'signedMessage', 'user'];
    const minifiedUser = {
      ...params,
      isFollowed: user && user.length > 0 ? user[0].isFollow : false,
    };
    for (const property in minifiedUser) {
      if (propertiesToRemove.includes(property)) {
        delete minifiedUser[property];
      }
    }
    return minifiedUser;
  }

  async findByPublicKey(publicKey: string): Promise<any> {
    const account = await this.prisma.user.findFirst({
      where: {
        publicKey: publicKey,
      },
    });
    if (!account) {
      return null;
    }
    return account;
  }

  async findOne(id: string) {
    const user = await this.prisma.user.findFirst({
      where: {
        id,
      },
    });
    if (!user) {
      throw new NotFoundException(`User #${id} not found`);
    }
    return user;
  }

  async findOneByWallet(address: string) {
    const user = await this.prisma.user.findUnique({
      where: {
        signer: address.toLowerCase(),
      },
    });
    if (!user) {
      throw new NotFoundException(`User #${address} not found`);
    }
    return user;
  }

  async findAll(
    filter: GetAllUser,
    currentUser: User,
  ): Promise<PagingResponse<any>> {
    const currentUserId = currentUser?.id;
    // const limit = (filter.limit || 12) as number;
    // const cursor = filter.cursor;
    // @ts-ignore
    // const take: number = limit && limit > 0 ? parseInt(limit) + 1 : 13;
    const whereCondition: any = {
      ...(filter.search
        ? {
            OR: [
              {
                username: {
                  contains: filter.search,
                  mode: 'insensitive',
                },
              },
              {
                email: {
                  contains: filter.search,
                  mode: 'insensitive',
                },
              },
              {
                signer: {
                  contains: filter.search,
                  mode: 'insensitive',
                },
              },
              {
                shortLink: {
                  contains: filter.search,
                  mode: 'insensitive',
                },
              },
              // Add more fields as needed
            ],
          }
        : {}),
      username: {
        not: null,
      },
    };

    const usersWithFollowStatus = await this.prisma.user.findMany({
      orderBy: {
        createdAt: filter.order,
      },
      where: whereCondition,
      skip: (filter.page - 1) * filter.limit,
      take: filter.limit,
      select: {
        id: true,
        email: true,
        avatar: true,
        username: true,
        signature: true,
        signedMessage: true,
        signDate: true,
        signer: true,
        acceptedTerms: true,
        createdAt: true,
        updatedAt: true,
        bio: true,
        facebookLink: true,
        twitterLink: true,
        telegramLink: true,
        shortLink: true,
        discordLink: true,
        webURL: true,
        coverImage: true,
        followers: true,
        following: true,
        ...(currentUserId
          ? {
              user: {
                select: {
                  isFollow: true,
                },
                where: {
                  followerId: currentUserId,
                },
              },
            }
          : {}),
      },
    });

    const total = await this.prisma.user.count({
      where: whereCondition,
    });
    const usersWithFollowStatusAndPaging = usersWithFollowStatus.map(
      ({ user, ...rest }) => ({
        ...rest,
        isFollowed: user && user.length > 0 ? user[0].isFollow : false,
      }),
    );
    // let nextCursor: string | null = null;
    // if (users.length > limit) {
    //   const nextUser = users.pop();
    //   nextCursor = nextUser.id;
    // }
    // return { users, nextCursor };

    return {
      data: usersWithFollowStatusAndPaging,
      paging: {
        total,
        page: filter.page,
        limit: filter.limit,
      },
    };
  }

  async getProfileWithShortLinkOrIdUser(input: string, user: any) {
    try {
      const currentUserId = user?.id;
      // const result: any = {};
      let isUuid = true;
      if (!isValidUUID(input)) {
        isUuid = false;
      }

      const result = await this.prisma.user.findFirst({
        where: {
          ...(isUuid
            ? { id: input }
            : {
                OR: [
                  { shortLink: { equals: input, mode: 'insensitive' } },
                  {
                    signer: {
                      equals: input.toLowerCase(),
                      mode: 'insensitive',
                    },
                  },
                ],
              }),
        },
        select: {
          id: true,
          email: true,
          avatar: true,
          username: true,
          signature: true,
          signedMessage: true,
          signDate: true,
          signer: true,
          acceptedTerms: true,
          createdAt: true,
          updatedAt: true,
          bio: true,
          facebookLink: true,
          twitterLink: true,
          telegramLink: true,
          shortLink: true,
          discordLink: true,
          webURL: true,
          coverImage: true,
          followers: true,
          following: true,
          ...(currentUserId
            ? {
                user: {
                  select: {
                    isFollow: true,
                  },
                  where: {
                    followerId: currentUserId,
                  },
                },
              }
            : {}),
        },
      });

      if (!result) {
        throw new NotFoundException();
      }

      const response =
        !currentUserId || currentUserId !== result.id
          ? this.minifyUserObject(result)
          : result;

      return response;
    } catch (error) {
      console.log(error);
      throw new HttpException(`${error.message}`, HttpStatus.BAD_REQUEST);
    }
  }

  async updateProfile(input: UpdateUserDto, user: User) {
    try {
      const { email, username, shortLink } = input;
      // Check for existing email
      if (email) {
        const checkExistEmail = await this.checkUserExistence(
          'email',
          email,
          user.id,
        );
        if (checkExistEmail) {
          throw new Error('Email already exists');
        }
      }
      // Check for existing username
      if (username) {
        const checkExistUsername = await this.checkUserExistence(
          'username',
          username,
          user.id,
        );
        if (checkExistUsername) {
          throw new Error('Username already exists');
        }
      }

      // Check for existing short link
      if (shortLink) {
        const checkExistShortLink = await this.checkUserExistence(
          'shortLink',
          shortLink,
          user.id,
        );
        if (checkExistShortLink) {
          throw new Error('Short Link already exists');
        }
      }

      const updateData = {
        email: input.email,
        username: input.username,
        acceptedTerms: input.acceptedTerms,
        bio: input.bio,
        facebookLink: input.facebookLink,
        twitterLink: input.twitterLink,
        telegramLink: input.telegramLink,
        discordLink: input.discordLink,
        webURL: input.webURL,
        coverImage: input.coverImage,
        shortLink: input.shortLink,
        avatar: input.avatar,
      };

      // Remove shortLink if not provided
      if (!shortLink) {
        delete updateData.shortLink;
      }

      return await this.prisma.user.update({
        where: {
          id: user.id,
        },
        data: updateData,
      });
    } catch (error) {
      throw new HttpException(`${error.message}`, HttpStatus.BAD_REQUEST);
    }
  }

  async checkUserExistence(field: string, value: string, userId: string) {
    const condition = {
      id: { not: userId },
      [field]: value,
    };

    const existingUser = await this.prisma.user.findFirst({
      where: { AND: [condition] },
    });

    return !!existingUser;
  }
  async getProjectByUser(
    query: findProjectsUserSubscribe,
    userId: string,
  ): Promise<ListProjectEntity> {
    try {
      const whereRounds: Prisma.ProjectRoundWhereInput = {};
      if (query.start) {
        whereRounds.start = { gte: new Date(query.start) };
      }

      if (query.end) {
        whereRounds.end = { lte: new Date(query.end) };
      }

      const user = await this.prisma.user.findFirst({
        where: {
          signer: userId.toLowerCase(),
        },
      });
      if (!user) {
        throw new NotFoundException('Subscriber not found');
      }
      const result = await this.prisma.userProject.findMany({
        where: {
          AND: [
            {
              userId: user.id,
            },
            {
              projectId: query.projectId,
            },
          ],
        },
        include: {
          project: {
            select: {
              id: true,
              idOnchain: true,
              name: true,
              banner: true,
              website: true,
              telegram: true,
              facebook: true,
              instagram: true,
              discord: true,
              shortLink: true,
              organization: true,
              description: true,
              isActivated: true,
              collection: true,
              details: true,
              twitter: true,
              logo: true,
              rounds: {
                where: whereRounds,
                include: {
                  round: true,
                },
              },
            },
          },
        },
      });
      const response = result.map((subcriber) => {
        const { project } = subcriber;
        return {
          ...subcriber,
          project: {
            ...project,
            rounds: project.rounds.map((round) => ({
              ...round,
              ...round.round,
            })),
          },
        };
      });

      const res = this.formatData(response);
      return res;
    } catch (error) {
      console.error(error);
      throw new HttpException(`${error.message}`, HttpStatus.BAD_REQUEST);
    }
  }
  async findActivityNFT(input: GetActivityBase) {
    try {
      const { user, page, limit, type } = input;
      const resultUser = await this.getUser(user);

      const or = [{ to: resultUser?.signer }, { from: resultUser?.signer }];
      const blocks = await this.activetiService.fetchActivityFromGraph({
        or,
        page,
        limit,
        type,
      });
      const result = await this.activetiService.processActivityUserData(blocks);
      return result;
    } catch (error) {
      console.log(error);
      throw new HttpException(`${error.message}`, HttpStatus.BAD_REQUEST);
    }
  }

  async getUser(user: any) {
    try {
      if (isValidUUID(user)) {
        // If user is a valid UUID, search by ID
        const responseUser = await this.prisma.user.findFirst({
          where: { id: user },
        });
        return responseUser;
      } else {
        // If user is not a valid UUID, search by username or shortLink
        const responseUser = await this.prisma.user.findFirst({
          where: {
            OR: [{ username: user }, { shortLink: user }, { signer: user }],
          },
        });
        return responseUser;
      }
    } catch (error) {
      console.log(error);
      throw new HttpException(`${error.message}`, HttpStatus.BAD_REQUEST);
    }
  }
  formatData(data) {
    if (data.length === 0) {
      throw new NotFoundException('User has not subscribed project');
    }
    const { userId, subscribeDate, stakingTotal, lastDateRecord, projectId } =
      data[0];
    const projects = data.map((item) => item?.project);

    return {
      userId,
      subscribeDate,
      stakingTotal,
      lastDateRecord,
      projectId,
      projects,
    };
  }

  async followUser(id: string, follower: User) {
    try {
      let isUuid = true;
      if (!isValidUUID(id)) {
        isUuid = false;
      }
      const user = await this.prisma.user.findFirst({
        where: {
          ...(isUuid ? { id } : { OR: [{ signer: id }, { shortLink: id }] }),
        },
      });
      if (!user) {
        throw new NotFoundException();
      }
      if (user.id == follower.id) {
        throw new Error('You cannot follow yourself');
      }
      const userFollowMatch = await this.prisma.userFollow.findFirst({
        where: {
          userId: user.id,
          followerId: follower.id,
        },
      });

      const existingFollow = await this.prisma.userFollow.upsert({
        where: {
          userId_followerId: {
            userId: user.id,
            followerId: follower.id,
          },
        },
        update: { isFollow: !userFollowMatch?.isFollow },
        create: {
          userId: user.id,
          followerId: follower.id,
          isFollow: true,
        },
      });

      const increment = !existingFollow.isFollow ? -1 : 1;
      // Followers
      await this.prisma.user.update({
        where: {
          id: user.id,
        },
        data: {
          followers: {
            increment,
          },
        },
      });
      // Following
      await this.prisma.user.update({
        where: {
          id: follower.id,
        },
        data: {
          following: {
            increment,
          },
        },
      });

      return { isFollowed: existingFollow.isFollow };
    } catch (error) {
      throw new HttpException(`${error.message}`, HttpStatus.BAD_REQUEST);
    }
  }
}
