import { CONTRACT_TYPE } from '@prisma/client';
import { PrismaService } from 'src/prisma/prisma.service';
import * as METRIC_JSON from '../../config/metric.json';
import {
  MetricCategory,
  MetricCollection,
  MetricCollectionJson,
  MetricUser,
  MetricUserJson,
  MetricNFT,
  MetricNFTJson,
  TypeCategory,
} from 'src/constants/enums/Metric.enum';
import { GetCollectionMarketData } from '../modules/graph-qlcaller/getCollectionMarketData.service';
import { logger } from 'src/commons';

interface CollectionGeneral {
  totalOwner: number;
  volumn: string;
  totalNft: number;
  // floorPrice: bigint;
}
class MetricCommon {
  constructor(
    private prisma: PrismaService,
    private readonly collectionData: GetCollectionMarketData,
  ) {}

  async handleMetric(
    typeCate: TypeCategory,
    metric: MetricCategory,
    id: string,
    collectionId?: string,
    nftId?: string,
  ) {
    switch (typeCate) {
      case TypeCategory.User:
        await this.handleMetricUser(metric, id, METRIC_JSON.User, collectionId);
        break;
      case TypeCategory.Collection:
        await this.handleMetricCollection(metric, id, METRIC_JSON.Collection);
        break;
      case TypeCategory.NFT:
        await this.handleMetricNFT(metric, id, METRIC_JSON.NFT);
        break;
    }
  }

  // Metric Point User: Metric Point Collection + Metric Point Verified + Metric Point Followers
  async handleMetricUser(
    metric: MetricCategory,
    id: string,
    metricUser: MetricUser,
    collectionId?: string,
  ) {
    try {
      const user = await this.prisma.user.findUnique({
        where: { id: id },
      });
      const metricDetail: MetricUserJson = JSON.parse(
        JSON.stringify(user.metricDetail),
      );
      switch (metric) {
        // when User verified plus metric point for category verified
        case MetricCategory.Verified:
          if (metricDetail.Verified <= 0) {
            metricDetail.Verified += metricUser.Verified.point;
            const metricPoint =
              BigInt(user?.metricPoint || 0) +
              BigInt(metricUser.Verified.point);
            await this.updateUserMetrics(id, metricPoint, metricDetail);
          }
          break;
        // This category about (totalItems, totalOwner, volume) Metric Point to add metric Point of user
        case MetricCategory.CollectionMetric:
          const collection = await this.prisma.collection.findUnique({
            where: {
              id: collectionId,
            },
          });
          let updatedMetricPointCM = user.metricPoint;
          if (metricDetail.CollectionMetric) {
            updatedMetricPointCM -= BigInt(metricDetail?.CollectionMetric);
          }
          const detailCMetric = { ...metricDetail };
          updatedMetricPointCM += BigInt(collection.metricPoint);
          detailCMetric.CollectionMetric = collection.metricPoint.toString();
          await this.updateUserMetrics(id, updatedMetricPointCM, detailCMetric);

          await this.handleMetric(
            TypeCategory.NFT,
            MetricCategory.UserMetric,
            user.id,
          );
          break;
        // This Category will plus with array check Point for metric followers
        // ex: 100 Follower -> + 1
        // ex: 200 Follower -> + 2
        case MetricCategory.Followers:
          let updatedMetricPoint = user.metricPoint;
          const updatedMetricDetail = { ...metricDetail };
          const totalFollowers = updatedMetricDetail?.Followers?.total || 0;
          if (totalFollowers > 0) {
            updatedMetricPoint -= BigInt(totalFollowers);
          }
          if (updatedMetricPoint < 0) {
            updatedMetricPoint = BigInt(0);
          }
          let sumMetricPointFollower = 0;
          let checkPointFollower = null;
          for (const pointFollow of metricUser.Followers) {
            if (user.followers < pointFollow.value) {
              break;
            }
            sumMetricPointFollower += pointFollow.point;
            checkPointFollower = { ...pointFollow };
          }
          if (checkPointFollower) {
            updatedMetricPoint += BigInt(sumMetricPointFollower);
            updatedMetricDetail.Followers = {
              ...checkPointFollower,
              total: sumMetricPointFollower,
            };
            await this.updateUserMetrics(
              id,
              updatedMetricPoint,
              updatedMetricDetail,
            );
            await this.handleMetric(
              TypeCategory.NFT,
              MetricCategory.UserMetric,
              user.id,
            );
          }
          break;
      }
    } catch (error) {
      console.log(error);
      logger.error(`handleMetricUser: ${JSON.stringify(error)}`);
    }
  }

  async handleMetricCollection(
    metric: MetricCategory,
    id: string,
    metricCollection: MetricCollection,
  ) {
    try {
      const collection = await this.prisma.collection.findUnique({
        where: { id: id },
      });
      const metricDetail: MetricCollectionJson = JSON.parse(
        JSON.stringify(collection.metricDetail),
      );
      switch (metric) {
        // when Collection verified plus metric point for category verified
        case MetricCategory.Verified:
          if (metricDetail.Verified <= 0) {
            metricDetail.Verified += metricCollection.Verified.point;
            const metricPoint =
              BigInt(collection?.metricPoint || 0) +
              BigInt(metricCollection.Verified.point);
            await this.updateCollectionMetrics(id, metricPoint, metricDetail);
          }
          if (collection.isVerified == false) {
            metricDetail.Verified -= metricCollection.Verified.point;
            const metricPoint =
              BigInt(collection?.metricPoint || 0) -
              BigInt(metricCollection.Verified.point);
            await this.updateCollectionMetrics(id, metricPoint, metricDetail);
          }
          break;
        // This category about (totalItems, totalOwner, volume) of Collection to check
        case MetricCategory.CollectionMetric:
          const { TotalItems, TotalUniqueOwner, Volume } = metricCollection;
          const { totalNft, totalOwner, volumn } =
            await this.getGeneralCollectionData(
              collection.address,
              collection.type,
            );
          let updatedMetricPoint = collection.metricPoint;
          if (metricDetail.TotalItems?.total)
            updatedMetricPoint -= BigInt(metricDetail.TotalItems.total);
          if (metricDetail.TotalUniqueOwner?.total)
            updatedMetricPoint -= BigInt(metricDetail.TotalUniqueOwner.total);
          if (metricDetail.Volume?.total)
            updatedMetricPoint -= BigInt(metricDetail.Volume.total);
          if (updatedMetricPoint < 0) {
            updatedMetricPoint = BigInt(0);
          }
          const updatedMetricDetail = { ...metricDetail };

          const [sumMetricPointTotalItems, checkPointTotalItems] =
            this.setcheckPointMetric(TotalItems, totalNft, 0);
          const [sumMetricPointTotalOwner, checkPointTotalOwner] =
            this.setcheckPointMetric(TotalUniqueOwner, totalOwner, 0);
          const [sumMetricPointVolume, checkPointVolume] =
            this.setcheckPointMetric(Volume, volumn, 0);
          // function check and add metric point
          const updateMetricDetail = (key, sumMetricPoint, checkPoint) => {
            if (checkPoint) {
              updatedMetricPoint += BigInt(sumMetricPoint);
              updatedMetricDetail[key] = {
                ...checkPoint,
                total: sumMetricPoint,
              };
            }
          };
          updateMetricDetail(
            'TotalItems',
            sumMetricPointTotalItems,
            checkPointTotalItems,
          );
          updateMetricDetail(
            'TotalUniqueOwner',
            sumMetricPointTotalOwner,
            checkPointTotalOwner,
          );
          updateMetricDetail('Volume', sumMetricPointVolume, checkPointVolume);

          await this.updateCollectionMetrics(
            collection.id,
            updatedMetricPoint,
            updatedMetricDetail,
          );

          const owner = await this.getOwnerCollection(id);
          await this.handleMetric(
            TypeCategory.User,
            MetricCategory.CollectionMetric,
            owner.userId,
            owner.collectionId,
          );
          break;
      }
    } catch (error) {
      console.log(error);
      logger.error(`handleMetricCollection: ${JSON.stringify(error)}`);
    }
  }

  // metric:
  //      Many Category to plus metric point
  // id :
  //      1. UserId to set Metric Point of User for NFT User Creator,
  //      2. nftId (Created In Base Collection) when sell success
  // metricNFT:
  //      Array check Point for metric
  async handleMetricNFT(
    metric: MetricCategory,
    id: string,
    metricNFT: MetricNFT,
  ) {
    try {
      const user = await this.prisma.user.findUnique({
        where: { id: id },
      });
      switch (metric) {
        case MetricCategory.UserMetric:
          const nft = await this.prisma.nFT.findMany({
            where: {
              creatorId: id,
            },
          });
          await Promise.all(
            nft.map(async (item) => {
              const metricDetail: MetricNFTJson = JSON.parse(
                JSON.stringify(item.metricDetail),
              );
              let updatedMetricPoint = item.metricPoint;
              if (metricDetail.UserMetric) {
                updatedMetricPoint -= BigInt(metricDetail.UserMetric);
                if (updatedMetricPoint < 0) {
                  updatedMetricPoint = BigInt(0);
                }
              }
              const updatedMetricDetail = { ...metricDetail };
              updatedMetricPoint += BigInt(user.metricPoint);
              updatedMetricDetail.UserMetric = user.metricPoint.toString();
              await this.updateNFTMetrics(
                item.id,
                item.collectionId,
                updatedMetricPoint,
                updatedMetricDetail,
              );
            }),
          );
          break;
        case MetricCategory.VolumeIndividual:
          break;
        default:
          break;
      }
    } catch (error) {
      console.log(error);
      logger.error(`handleMetricCollection: ${JSON.stringify(error)}`);
    }
  }

  async getGeneralCollectionData(
    collectionAddress: string,
    type: CONTRACT_TYPE,
  ): Promise<CollectionGeneral> {
    if (!collectionAddress) {
      return {
        volumn: '0',
        totalOwner: Number(0),
        totalNft: Number(0),
      };
    }
    const [statusCollection] = await Promise.all([
      this.collectionData.getCollectionCount(collectionAddress),
    ]);

    if (type === 'ERC721') {
      return {
        volumn: statusCollection.erc721Contract?.volume || 0,
        totalOwner: statusCollection.erc721Contract?.holderCount || 0,
        totalNft: statusCollection.erc721Contract?.count || 0,
      };
    } else {
      return {
        volumn: statusCollection.erc721Contract?.volume || 0,
        totalOwner: statusCollection.erc1155Contract?.holderCount || 0,
        totalNft: statusCollection.erc1155Contract?.count || 0,
      };
    }
  }

  async getOwnerCollection(id: string) {
    try {
      const owner = await this.prisma.userCollection.findFirst({
        where: {
          collectionId: id,
        },
      });

      return owner;
    } catch (error) {
      logger.error(`getOwnerCollection: ${JSON.stringify(error)}`);
    }
  }
  setcheckPointMetric(metric, total, sumMetricPoint) {
    let checkPoint = null;
    for (const point of metric) {
      if (total < point.value) break;
      sumMetricPoint += point.point;
      checkPoint = { ...point };
    }
    return [sumMetricPoint, checkPoint];
  }

  async updateUserMetrics(
    id: string,
    metricPoint: bigint,
    metricDetail: MetricUserJson,
  ) {
    try {
      await this.prisma.user.update({
        data: {
          metricPoint: metricPoint,
          metricDetail: JSON.parse(JSON.stringify(metricDetail)),
        },
        where: { id },
      });
    } catch (error) {
      logger.error(`updateUserMetrics: ${error}`);
    }
  }

  async updateCollectionMetrics(
    id: string,
    metricPoint: bigint,
    metricDetail: MetricCollectionJson,
  ) {
    try {
      await this.prisma.collection.update({
        data: {
          metricPoint: metricPoint,
          metricDetail: JSON.parse(JSON.stringify(metricDetail)),
        },
        where: { id },
      });
    } catch (error) {
      logger.error(`updateCollectionMetrics: ${JSON.stringify(error)}`);
    }
  }

  async updateNFTMetrics(
    id: string,
    collectionId: string,
    metricPoint: bigint,
    metricDetail: MetricNFTJson,
  ) {
    try {
      await this.prisma.nFT.update({
        data: {
          metricPoint: metricPoint,
          metricDetail: JSON.parse(JSON.stringify(metricDetail)),
        },
        where: {
          id_collectionId: {
            id: id,
            collectionId: collectionId,
          },
        },
      });
    } catch (error) {
      logger.error(`updateCollectionMetrics: ${JSON.stringify(error)}`);
    }
  }
}

export default new MetricCommon(
  new PrismaService(),
  new GetCollectionMarketData(),
);
