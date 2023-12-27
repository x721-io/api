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
  private minifyUserObject(user: any): any {
    const propertiesToRemove = ['signature', 'signer', 'signedMessage'];
    const minifiedUser = { ...user };
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

  async findAll(filter: GetAllUser): Promise<PagingResponse<UserEntity>> {
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

    const users = await this.prisma.user.findMany({
      orderBy: {
        createdAt: filter.order,
      },
      where: whereCondition,
      // take: take,
      // cursor: cursor ? { id: cursor } : undefined,
      // skip: cursor ? 0 : 0,
      skip: (filter.page - 1) * filter.limit,
      take: filter.limit,
    });

    const total = await this.prisma.user.count({
      where: whereCondition,
    });
    // let nextCursor: string | null = null;
    // if (users.length > limit) {
    //   const nextUser = users.pop();
    //   nextCursor = nextUser.id;
    // }
    // return { users, nextCursor };

    return {
      data: users,
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
      let result: any = {};

      if (!isValidUUID(input)) {
        result = await this.prisma.user.findFirst({
          where: {
            OR: [
              { shortLink: { equals: input, mode: 'insensitive' } },
              { signer: { equals: input.toLowerCase(), mode: 'insensitive' } },
            ],
          },
        });
      } else {
        result = await this.prisma.user.findFirst({
          where: { id: input },
        });
      }

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
    query: FindAllProjectDto,
    user: User,
  ): Promise<ListProjectEntity[]> {
    try {
      const whereRounds: Prisma.ProjectRoundWhereInput = {};

      if (query.start) {
        whereRounds.start = { gte: new Date(query.start) };
      }

      if (query.end) {
        whereRounds.end = { lte: new Date(query.end) };
      }
      const result = await this.prisma.userProject.findMany({
        where: {
          userId: user.id,
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
      return response;
    } catch (error) {
      throw new HttpException(`${error.message}`, HttpStatus.BAD_REQUEST);
    }
  }
  async findActivityNFT(input: GetActivityBase) {
    try {
      const { to, from, page, limit, type } = input;
      const or = [{ to }, { from }];
      const blocks = await this.activetiService.fetchActivityFromGraph({
        or,
        page,
        limit,
        type,
      });
      const result = await this.activetiService.processActivityNFTData(blocks);
      return result;
    } catch (error) {
      console.log(error);
      throw new HttpException(`${error.message}`, HttpStatus.BAD_REQUEST);
    }
  }
}
