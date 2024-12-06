import {
  Injectable,
  HttpException,
  HttpStatus,
  NotFoundException,
  Inject,
} from '@nestjs/common';
import {
  UpdateAccountDto,
  UpdatePasswordDto,
  ResetPasswordDtop,
  UpdateRolesDto,
} from '../dto/update-account.dto';
import {
  ActiveNFTDto,
  ActiveCollectionDto,
  ActiveUserDto,
  VerifyCollectionDto,
} from '../dto/marketplace.dto';
import { PrismaService } from 'src/prisma/prisma.service';
import { JwtService } from '@nestjs/jwt';
import { ConfigService } from '@nestjs/config';
import OtherCommon from 'src/commons/Other.common';
import { CreateAccountDto } from '../dto/create-account.dto';
import { GetAllAccountDto } from '../dto/get-all-account.dto';
import { validate as isValidUUID } from 'uuid';
import { GetCollectionMarketData } from '../../graph-qlcaller/getCollectionMarketData.service';
import { Account, Prisma, TX_STATUS } from '@prisma/client';
import PaginationCommon from 'src/commons/HasNext.common';
import { accountListSelect } from '../../../commons/definitions/Constraint.Object';
import MetricCommon from 'src/commons/Metric.common';
import { MetricCategory, TypeCategory } from 'src/constants/enums/Metric.enum';
import { GetSummaryDto } from '../dto/cms.dto';
import { GraphQlcallerService } from 'src/modules/graph-qlcaller/graph-qlcaller.service';
import { EventType } from 'src/generated/graphql';
import { ethers } from 'ethers';
import SecureUtil from '../../../commons/Secure.common';
import {
  CreateCollectionExternalDto,
  Game,
  WebhookCollectionDto,
} from '../dto/create-collection-external.dto';
import { UserService } from 'src/modules/user/user.service';
interface CountTransactionDto {
  start: number;
  end: number;
  event: EventType;
}

interface SummmaryAllResponse {
  countCollection: number;
  countNFT: number;
  countUser: number;
  countTrade: number;
  countAcceptBid: number;
  countBid: number;
  countVolume721: number;
  countVolume1155: number;
}

@Injectable()
export class CMSService {
  constructor(
    private readonly prisma: PrismaService,
    private readonly GraphqlService: GraphQlcallerService,
    private userService: UserService,
  ) {}

  async findAll(filter: GetAllAccountDto): Promise<PagingResponseHasNext<any>> {
    try {
      const whereCondition: Prisma.AccountWhereInput = {
        username: filter.username,
      };
      const listAccount = await this.prisma.account.findMany({
        skip: (filter.page - 1) * filter.limit,
        take: filter.limit,
        where: whereCondition,
        orderBy: {
          createdAt: filter.order,
        },
        select: accountListSelect,
      });
      const hasNext = await PaginationCommon.hasNextPage(
        filter.page,
        filter.limit,
        'account',
        whereCondition,
      );
      return {
        data: listAccount,
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

  async findOne(id: string) {
    try {
      if (!isValidUUID(id)) {
        throw new Error('Invalid User. Please try again !');
      }
      const user = await this.prisma.account.findUnique({
        where: {
          id: id,
        },
      });
      delete user.password;
      return user;
    } catch (error) {
      throw new HttpException(`${error.message}`, HttpStatus.BAD_REQUEST);
    }
  }

  async findRolesByUser(id: string) {
    try {
      if (!isValidUUID(id)) {
        throw new Error('Invalid User. Please try again !');
      }
      const user = await this.prisma.account.findUnique({
        where: {
          id: id,
        },
      });
      return user.roles;
    } catch (error) {
      throw new HttpException(`${error.message}`, HttpStatus.BAD_REQUEST);
    }
  }
  async create(createAccountDto: CreateAccountDto) {
    try {
      const checkUserName = await this.prisma.account.findUnique({
        where: {
          username: createAccountDto.username,
        },
      });
      if (checkUserName) {
        throw new Error('Username is already exists');
      }

      const passwordHash = await OtherCommon.createHashPassword(
        createAccountDto.password,
      );
      const dataCreate: Prisma.AccountCreateInput = {
        username: createAccountDto.username,
        email: createAccountDto.email,
        password: passwordHash,
        avatar: createAccountDto.avatar,
        twitterLink: createAccountDto.twitterLink,
        telegramLink: createAccountDto.telegramLink,
        phone: createAccountDto.phone,
        roles: createAccountDto.roles,
        fullName: createAccountDto.fullName,
      };
      const newAccount = await this.prisma.account.create({
        data: dataCreate,
      });
      if (!newAccount) {
        throw new Error('Initialization failed');
      }
      return newAccount;
    } catch (error) {
      throw new HttpException(`${error.message}`, HttpStatus.BAD_REQUEST);
    }
  }

  async update(updateAccountDto: UpdateAccountDto, account: Account) {
    try {
      const checkExistUser = await this.prisma.account.findUnique({
        where: {
          id: account.id,
        },
      });
      if (!checkExistUser) {
        throw new NotFoundException();
      }
      const UpdateAccount = await this.prisma.account.update({
        data: {
          email: updateAccountDto.email,
          avatar: updateAccountDto.avatar,
          twitterLink: updateAccountDto.twitterLink,
          telegramLink: updateAccountDto.telegramLink,
          phone: updateAccountDto.phone,
          fullName: updateAccountDto.fullName,
        },
        where: {
          id: account.id,
        },
      });
      if (!UpdateAccount) {
        // throw new ForbiddenException();
        throw Error('Error! An error occurred. Please try again later');
      }
      return UpdateAccount;
      // throw Error('Error! An error occurred. Please try again later');
    } catch (error) {
      throw new HttpException(`${error.message}`, HttpStatus.BAD_REQUEST);
    }
  }

  async UpdatePassword(updatePasswordDto: UpdatePasswordDto, account: Account) {
    try {
      const checkExistAccount = await this.prisma.account.findUnique({
        where: {
          id: account.id,
        },
      });
      if (!checkExistAccount) {
        throw new NotFoundException();
      }
      const verify = await OtherCommon.verifyPassword(
        updatePasswordDto.currentPassword,
        checkExistAccount.password,
      );
      if (!verify) {
        throw new Error('Your current password is incorrect, please try again');
      }
      const newPasswordHash = await OtherCommon.createHashPassword(
        updatePasswordDto.newPassword,
      );
      const resultUpdate = await this.prisma.account.update({
        data: {
          password: newPasswordHash,
        },
        where: {
          id: account.id,
        },
      });
      delete resultUpdate.password;
      return resultUpdate;
    } catch (error) {
      throw new HttpException(`${error.message}`, HttpStatus.BAD_REQUEST);
    }
  }

  async ResetPassword(resetPasswordDtop: ResetPasswordDtop, account: Account) {
    try {
      if (!isValidUUID(resetPasswordDtop.id)) {
        throw new Error('Invalid User. Please try again !');
      }
      const checkExistAccount = await this.prisma.account.findUnique({
        where: {
          id: resetPasswordDtop.id,
        },
      });

      if (!checkExistAccount) {
        throw new NotFoundException();
      }
      const resetPassword = await OtherCommon.createHashPassword(
        resetPasswordDtop.newPassword,
      );

      const resultResetPassword = await this.prisma.account.update({
        data: {
          password: resetPassword,
        },
        where: {
          id: checkExistAccount.id,
        },
      });

      await this.handleActionLog(
        account.id,
        `Account ${account.id} Reset password account: ${resetPasswordDtop.id}`,
      );

      delete resultResetPassword.password;
      return resultResetPassword;
    } catch (error) {
      throw new HttpException(`${error.message}`, HttpStatus.BAD_REQUEST);
    }
  }

  async ChangeRolesAccount(updateRolesDto: UpdateRolesDto) {
    try {
      if (!isValidUUID(updateRolesDto.id)) {
        throw new Error('Invalid User. Please try again !');
      }

      const updateRoleAccount = await this.prisma.account.update({
        data: {
          roles: updateRolesDto.roles,
        },
        where: {
          id: updateRolesDto.id,
        },
      });

      if (!updateRoleAccount) {
        throw Error('Error! An error occurred. Please try again later');
      }
      return true;
    } catch (error) {
      throw new HttpException(`${error.message}`, HttpStatus.BAD_REQUEST);
    }
  }

  async handleActiveNFT(activeNFTDto: ActiveNFTDto, account: Account) {
    try {
      const { collectionId, id } = activeNFTDto;
      const collection = await this.prisma.collection.findUnique({
        where: {
          id: collectionId.toLowerCase(),
        },
      });

      if (!collection) {
        throw new NotFoundException('No collection was found');
      }

      const nft = await this.prisma.nFT.findUnique({
        where: {
          id_collectionId: {
            id: id,
            collectionId: collection.id,
          },
        },
      });
      if (!nft) {
        throw new NotFoundException('No NFT was found');
      }
      await this.handleActionLog(
        account.id,
        ` Account: ${account.id} set NFT: ${collectionId}/${id} - active: ${activeNFTDto.isActive}`,
      );
      return await this.prisma.nFT.update({
        data: {
          isActive: activeNFTDto.isActive,
        },
        where: {
          id_collectionId: {
            id,
            collectionId: collection.id,
          },
        },
      });
    } catch (error) {
      console.log(error);
      throw new HttpException(`${error.message}`, HttpStatus.BAD_REQUEST);
    }
  }

  async handleActiveCollection(
    activeCollectionDto: ActiveCollectionDto,
    account: Account,
  ) {
    try {
      const collection = await this.prisma.collection.findUnique({
        where: {
          id: activeCollectionDto.id,
        },
      });
      if (!collection) {
        throw new NotFoundException();
      }
      await this.handleActionLog(
        account.id,
        ` Account: ${account.id} set NFT: ${activeCollectionDto.id} - active: ${activeCollectionDto.isActive}`,
      );
      return await this.prisma.collection.update({
        data: {
          isActive: activeCollectionDto.isActive,
        },
        where: {
          id: activeCollectionDto.id,
        },
      });
    } catch (error) {
      throw new HttpException(`${error.message}`, HttpStatus.BAD_REQUEST);
    }
  }

  async handleActiveUser(activeUserDto: ActiveUserDto, account: Account) {
    try {
      const user = await this.prisma.user.findUnique({
        where: {
          id: activeUserDto.id,
        },
      });
      if (!user) {
        throw new NotFoundException();
      }
      await this.handleActionLog(
        account.id,
        ` Account: ${account.id} Set active user: ${activeUserDto.id} - active: ${activeUserDto.isActive}`,
      );
      return await this.prisma.user.update({
        data: {
          isActive: activeUserDto.isActive,
        },
        where: {
          id: activeUserDto.id,
        },
      });
    } catch (error) {
      throw new HttpException(`${error.message}`, HttpStatus.BAD_REQUEST);
    }
  }
  async handleActionLog(id: string, message: string) {
    try {
      await this.prisma.logAction.create({
        data: {
          detail: message,
          accountId: id,
        },
      });
    } catch (error) {
      throw new HttpException(`${error.message}`, HttpStatus.BAD_REQUEST);
    }
  }

  async handleVerifyCollection(
    verifyCollectionDto: VerifyCollectionDto,
    account: Account,
  ) {
    try {
      const collection = await this.prisma.collection.findUnique({
        where: {
          id: verifyCollectionDto.id,
        },
      });
      if (!collection) {
        throw new NotFoundException();
      }
      await this.handleActionLog(
        account.id,
        ` Account: ${account.id} set NFT: ${verifyCollectionDto.id} - Verified: ${verifyCollectionDto.isVerified}`,
      );

      const result = await this.prisma.collection.update({
        data: {
          isVerified: verifyCollectionDto.isVerified,
        },
        where: {
          id: verifyCollectionDto.id,
        },
      });

      await MetricCommon.handleMetric(
        TypeCategory.Collection,
        MetricCategory.Verified,
        result.id,
      );
      return result;
    } catch (error) {
      throw new HttpException(`${error.message}`, HttpStatus.BAD_REQUEST);
    }
  }

  async getRolesAccount(account: Account) {
    try {
      return account.roles || [];
    } catch (error) {
      throw new HttpException(`${error.message}`, HttpStatus.BAD_REQUEST);
    }
  }

  async deleteAccount(id: string, account: Account) {
    try {
      if (!isValidUUID(id)) {
        throw new Error('Invalid User. Please try again !');
      }
      const checkExist = await this.prisma.account.findUnique({
        where: {
          id: id,
        },
      });
      if (!checkExist) {
        throw new NotFoundException();
      }
      await this.handleActionLog(
        account.id,
        ` Account: ${account.id} Delete Account: ${id}`,
      );
      return await this.prisma.account.update({
        data: {
          isDelete: true,
          isActive: false,
        },
        where: {
          id: id,
        },
      });
    } catch (error) {
      throw new HttpException(`${error.message}`, HttpStatus.BAD_REQUEST);
    }
  }

  async getSummary(input: GetSummaryDto) {
    try {
      const now = new Date();
      const end = input?.end
        ? input?.end
        : this.getLastDay(now.getFullYear(), now.getMonth());
      const start = input?.start
        ? input?.start
        : this.getFirstDay(now.getFullYear(), now.getMonth());

      const cacheKey = `summary_${this.toTimestamp(start)}_${this.toTimestamp(
        end,
      )}`;
      const cachedSummary = await SecureUtil.getSessionInfo(cacheKey);
      if (cachedSummary) {
        return JSON.parse(cachedSummary);
      }

      const collectionWhere: Prisma.CollectionWhereInput = {
        AND: [
          {
            createdAt: {
              gte: start,
              lte: end,
            },
          },
          {
            status: 'SUCCESS',
          },
        ],
      };

      const nftWhere: Prisma.NFTWhereInput = {
        AND: [
          {
            createdAt: {
              gte: start,
              lte: end,
            },
          },
          {
            status: 'SUCCESS',
          },
        ],
      };

      const userWhere: Prisma.UserWhereInput = {
        createdAt: {
          gte: start,
          lte: end,
        },
      };
      const [
        countCollection,
        countNFT,
        countUser,
        countTrade,
        countBid,
        countAcceptBid,
        volume721vs1155,
        summaryAll,
      ] = await Promise.all([
        this.prisma.collection.count({ where: collectionWhere }),
        this.prisma.nFT.count({ where: nftWhere }),
        this.prisma.user.count({ where: userWhere }),
        this.getCountTransaction({
          start: this.toTimestamp(start),
          end: this.toTimestamp(end),
          event: EventType.Trade,
        }),
        this.getCountTransaction({
          start: this.toTimestamp(start),
          end: this.toTimestamp(end),
          event: EventType.Bid,
        }),
        this.getCountTransaction({
          start: this.toTimestamp(start),
          end: this.toTimestamp(end),
          event: EventType.AcceptBid,
        }),
        this.getCountVolume(),
        this.getSummaryAll(),
      ]);

      const responseResult = {
        summary: {
          countCollection,
          countNFT,
          countUser,
          countTrade,
          countAcceptBid,
          countBid,
          ...volume721vs1155,
        },
        summaryAll: summaryAll,
      };

      // Store the result in cache
      await SecureUtil.storeObjectSession(cacheKey, responseResult, 600);

      return responseResult;
    } catch (error) {
      console.log(error);
      throw new HttpException(`${error.message}`, HttpStatus.BAD_REQUEST);
    }
  }

  getLastDay(year, month) {
    const date = new Date(Date.UTC(year, month + 1, 0));
    date.setHours(23, 59, 59);
    return date;
  }

  getFirstDay(year, month) {
    const date = new Date(Date.UTC(year, month, 1));
    return date;
  }

  toTimestamp(strDate) {
    const datum = Date.parse(strDate);
    return datum / 1000;
  }

  // async getCountTransaction(input: CountTransactionDto) {
  //   try {
  //     let hasMore = true;
  //     const batchSize = 1000;
  //     let offset = 0;
  //     let count = 0;
  //     while (hasMore) {
  //       const result = await this.GraphqlService.getSummaryTransaction(
  //         input.event,
  //         offset,
  //         batchSize,
  //         input.start,
  //         input.end,
  //       );
  //       if (result && result?.blocks && result?.blocks?.length > 0) {
  //         count += result?.blocks?.length;
  //         offset += batchSize;
  //       } else {
  //         hasMore = false;
  //       }
  //     }
  //     return count;
  //   } catch (error) {
  //     throw new HttpException(`${error.message}`, HttpStatus.BAD_REQUEST);
  //   }
  // }
  async getCountTransaction(input: CountTransactionDto) {
    try {
      let hasMore = true;
      const batchSize = 1000;
      let offset = 0;
      let count = 0;
      const maxConcurrentRequests = 20; // Number of concurrent requests
      const promises = [];

      while (hasMore) {
        const fetchBatch = async (currentOffset: number) => {
          const result = await this.GraphqlService.getSummaryTransaction(
            input.event,
            currentOffset,
            batchSize,
            input.start,
            input.end,
          );
          return result;
        };

        for (let i = 0; i < maxConcurrentRequests && hasMore; i++) {
          promises.push(fetchBatch(offset + i * batchSize));
        }

        const results = await Promise.all(promises);

        results.forEach((result) => {
          if (result && result.blocks && result.blocks.length > 0) {
            count += result.blocks.length;
          } else {
            hasMore = false;
          }
        });

        offset += batchSize * maxConcurrentRequests;
        promises.length = 0; // Clear the promises array for the next batch
      }

      return count;
    } catch (error) {
      throw new HttpException(`${error.message}`, HttpStatus.BAD_REQUEST);
    }
  }

  async getCountVolume() {
    try {
      const volume721 = await this.GraphqlService.getSummaryVolume(
        process.env.ADDRESS_ERC721_MARKET_CONTRACT.toLowerCase(),
      );
      const volume1155 = await this.GraphqlService.getSummaryVolume(
        process.env.ADDRESS_ERC1155_MARKET_CONTRACT.toLowerCase(),
      );
      if (
        (volume721 && !volume721?.marketVolume) ||
        (volume1155 && !volume721?.marketVolume)
      ) {
        return {
          countVolume721: 0,
          countVolume1155: 0,
        };
      }
      const countVolume721 = parseFloat(
        ethers.formatEther(volume721?.marketVolume?.totalVolume || 0),
      );
      const countVolume1155 = parseFloat(
        ethers.formatEther(volume1155?.marketVolume?.totalVolume || 0),
      );

      return {
        countVolume721: isNaN(countVolume721) ? 0 : countVolume721,
        countVolume1155: isNaN(countVolume1155) ? 0 : countVolume1155,
      };
    } catch (error) {
      throw new HttpException(`${error.message}`, HttpStatus.BAD_REQUEST);
    }
  }

  async getSummaryAll() {
    try {
      const resultSummary = await SecureUtil.getSessionInfo(`summary-cms`);
      const result: SummmaryAllResponse = JSON.parse(resultSummary);
      return result;
    } catch (error) {
      throw new HttpException(`${error.message}`, HttpStatus.BAD_REQUEST);
    }
  }

  async createCollectionExternal(
    input: CreateCollectionExternalDto,
  ): Promise<any> {
    try {
      const checkExist = await this.prisma.collection.findFirst({
        where: {
          OR: [
            { txCreationHash: input.txCreationHash },
            { name: input.name },
            { shortUrl: input.shortUrl },
          ],
        },
      });
      if (checkExist) {
        throw new Error(
          'Transaction hash or name or Short URL are already exists',
        );
      }
      if (!isValidUUID(input.userId)) {
        throw new Error('Invalid owner. Please try again !');
      }

      const checkUser = await this.prisma.user.findUnique({
        where: {
          id: input.userId,
        },
      });

      if (!checkUser) {
        throw new NotFoundException();
      }
      const collection = await this.prisma.collection.create({
        data: {
          txCreationHash: input.txCreationHash,
          name: input.name,
          symbol: input.symbol,
          description: input.description,
          status: TX_STATUS.SUCCESS,
          type: input.type,
          ...(input.shortUrl && { shortUrl: input.shortUrl }),
          coverImage: input.coverImage,
          metadata: input.metadata,
          avatar: input.avatar,
          flagExtend: input.flagExternal,
          subgraphUrl: input.subgraphUrl,
          ...(input.categoryId && { categoryId: Number(input.categoryId) }),
        },
      });

      await this.prisma.userCollection.create({
        data: {
          userId: input.userId,
          collectionId: collection.id,
        },
      });
      return collection;
    } catch (error) {
      throw new HttpException(`${error.message}`, HttpStatus.BAD_REQUEST);
    }
  }

  async webhookCreateCollection(input: WebhookCollectionDto): Promise<any> {
    try {
      const checkExist = await this.prisma.collection.findFirst({
        where: {
          OR: [
            { txCreationHash: input.txCreationHash },
            { name: input.name },
            { shortUrl: input.shortUrl },
          ],
        },
      });
      if (checkExist) {
        throw new Error(
          'Transaction hash or name or Short URL are already exists',
        );
      }
      const checkUser = await this.userService.fetchOrCreateUser(
        input.creatorAddress,
      );

      const metaJson = input.meta as unknown as Prisma.JsonObject;
      const collection = await this.prisma.collection.create({
        data: {
          txCreationHash: input.txCreationHash,
          name: input.name,
          symbol: input.symbol,
          description: input.description,
          status: TX_STATUS.SUCCESS,
          type: input.type,
          ...(input.shortUrl && { shortUrl: input.shortUrl }),
          coverImage: input.coverImage,
          metadata: input.metadata,
          avatar: input.avatar,
          flagExtend: input.flagExternal,
          subgraphUrl: input.subgraphUrl,
          ...(input.categoryId && { categoryId: Number(input.categoryId) }),
          address: input?.address,
          metadataJson: metaJson,
          gameId: input.gameId,
          source: input?.source,
        },
      });
      await this.prisma.userCollection.create({
        data: {
          userId: checkUser.id,
          collectionId: collection.id,
        },
      });
      return collection;
    } catch (error) {
      throw new HttpException(`${error.message}`, HttpStatus.BAD_REQUEST);
    }
  }
}
