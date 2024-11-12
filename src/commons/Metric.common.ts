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

  // Method to handle various metrics based on category and type
  async handleMetric(
    typeCate: TypeCategory,
    metric: MetricCategory,
    id: string,
    collectionId?: string,
  ) {
    switch (typeCate) {
      case TypeCategory.User:
        await this.handleMetricUser(metric, id, METRIC_JSON.User, collectionId);
        break;
      case TypeCategory.Collection:
        await this.handleMetricCollection(metric, id, METRIC_JSON.Collection);
        break;
      case TypeCategory.NFT:
        await this.handleMetricNFT(metric, id);
        break;
      default:
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
        // Handling verified user metric
        case MetricCategory.Verified:
          if (metricDetail.Verified <= 0) {
            metricDetail.Verified += metricUser.Verified.point;
            const metricPoint =
              BigInt(user?.metricPoint || 0) +
              BigInt(metricUser.Verified.point);
            await this.updateUserMetrics(id, metricPoint, metricDetail);
          }
          break;
        // Handling collection metric for user (totalItems, totalOwner, volume)
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
        // Handling followers metric for user
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
        default:
          break;
      }
    } catch (error) {
      console.log(error);
      logger.error(`handleMetricUser: ${JSON.stringify(error)}`);
    }
  }

  // Method to handle collection metrics
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
        // Handling verified collection metric
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
        // Handling collection metrics (totalItems, totalOwner, volume)
        case MetricCategory.CollectionMetric:
          const { TotalItems, TotalUniqueOwner, Volume } = metricCollection;
          const { totalNft, totalOwner, volumn } =
            await this.getGeneralCollectionData(
              collection.address,
              collection.type,
              collection?.flagExtend,
              collection?.volumeWei,
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
        default:
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
  async handleMetricNFT(metric: MetricCategory, id: string) {
    try {
      const user = await this.prisma.user.findUnique({
        where: { id: id },
      });
      switch (metric) {
        // Handling user metric for NFTs
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
              await this.NFTMetricsMarketplaceStatus(
                item.id,
                item.collectionId,
                updatedMetricPoint,
              );
            }),
          );
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
    flagExtend = false,
    volumeWei: string,
  ): Promise<CollectionGeneral> {
    if (!collectionAddress) {
      return {
        volumn: '0',
        totalOwner: Number(0),
        totalNft: Number(0),
        // floorPrice: BigInt(0),
      };
    }
    let totalNftExternal = 0;
    let totalOwnerExternal = 0;

    if (!!flagExtend) {
      const resultExternal =
        await this.collectionData.getAllCollectionExternal(collectionAddress);
      totalNftExternal = resultExternal.totalNftExternal;
      totalOwnerExternal = resultExternal.totalOwnerExternal;
    }

    const [statusCollection, contractOwner] = await Promise.all([
      this.collectionData.getCollectionCount(collectionAddress),
      this.collectionData.getContractInfor(collectionAddress),
      // this.getVolumeCollection(collectionAddress),
    ]);

    if (type === 'ERC721') {
      return {
        // volumn: statusCollection.erc721Contract?.volume || 0,
        volumn: volumeWei || `0`,
        totalOwner: !!flagExtend
          ? totalOwnerExternal
          : contractOwner?.contract?.count || 0,
        totalNft: !!flagExtend
          ? totalNftExternal
          : statusCollection.erc721Contract?.count || 0,
      };
    } else {
      return {
        // volumn: statusCollection.erc1155Contract?.volume || 0,
        volumn: volumeWei || `0`,
        totalOwner: !!flagExtend
          ? totalOwnerExternal
          : contractOwner?.contract?.count || 0,
        totalNft: !!flagExtend
          ? totalNftExternal
          : statusCollection.erc1155Contract?.count || 0,
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
  // Method to set check point metric
  setcheckPointMetric(metric, total, sumMetricPoint) {
    let checkPoint = null;
    for (const point of metric) {
      if (total < point.value) break;
      sumMetricPoint += point.point;
      checkPoint = { ...point };
    }
    return [sumMetricPoint, checkPoint];
  }

  // Method to update user metrics
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

  // Method to update collection metrics
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

  // Method to update NFT metrics
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
      logger.error(`updateNFTMetrics: ${JSON.stringify(error)}`);
    }
  }

  // Method to NFTMetricsMarketplaceStatus
  async NFTMetricsMarketplaceStatus(
    NftId: string,
    collectionId: string,
    metricPoint: bigint,
  ) {
    console.log({
      metricPoint,
      NftId,
      collectionId,
    });
    try {
      await this.prisma.marketplaceStatus.updateMany({
        where: {
          tokenId: NftId,
          collectionId: collectionId,
        },
        data: {
          metricPoint: BigInt(metricPoint),
        },
      });
    } catch (error) {
      logger.error(`updateNFTMetrics: ${JSON.stringify(error)}`);
    }
  }

  async handleMetricFreeMint(
    id: string,
    collectionId: string,
    creatorId: string,
    prices: string,
  ) {
    try {
      const { VolumeIndividual } = METRIC_JSON.NFT;
      const [user, nft] = await Promise.all([
        this.prisma.user.findUnique({
          where: {
            id: creatorId,
          },
        }),
        this.prisma.nFT.findUnique({
          where: {
            id_collectionId: {
              id: id,
              collectionId: collectionId,
            },
          },
        }),
      ]);

      const metricDetailUser: MetricUserJson = JSON.parse(
        JSON.stringify(user.metricDetail),
      );

      const metricDetailNFT: MetricNFTJson = JSON.parse(
        JSON.stringify(nft.metricDetail),
      );

      let metricPointUser = user.metricPoint;
      let metricPointNFT = nft.metricPoint;

      const updatedMetricDetailUser = { ...metricDetailUser };
      const updatedMetricDetailNFT = { ...metricDetailNFT };

      let totalVIUser = BigInt(updatedMetricDetailUser.VolumeIndividual);
      let totalVINFT = BigInt(updatedMetricDetailNFT.VolumeIndividual);

      // if (BigInt(updatedMetricDetailNFT.VolumeIndividual) > BigInt(0)) {
      //   metricPointUser -= BigInt(totalVIUser);
      // }
      // if (metricPointUser < 0) {
      //   metricPointUser = BigInt(0);
      // }

      // if (BigInt(updatedMetricDetailNFT.VolumeIndividual) > BigInt(0)) {
      //   metricPointNFT -= BigInt(totalVINFT);
      // }
      // if (metricPointNFT < 0) {
      //   metricPointNFT = BigInt(0);
      // }

      for (const point of VolumeIndividual) {
        if (prices < point.value) break;
        metricPointUser += BigInt(point.point);
        metricPointNFT += BigInt(point.point);
        totalVIUser += BigInt(point.point);
        totalVINFT += BigInt(point.point);
      }

      // Update Metric User
      updatedMetricDetailUser.VolumeIndividual = totalVIUser.toString();
      await this.updateUserMetrics(
        user.id,
        metricPointUser,
        updatedMetricDetailUser,
      );

      // Update Metric NFT
      updatedMetricDetailNFT.VolumeIndividual = totalVINFT.toString();
      // Update Metric For NFT
      await this.updateNFTMetrics(
        nft.id,
        nft.collectionId,
        metricPointNFT,
        updatedMetricDetailNFT,
      );

      // // Update Metric User for All NFT this User Creator
      await this.handleMetric(
        TypeCategory.NFT,
        MetricCategory.UserMetric,
        user.id,
      );
    } catch (error) {
      console.log(error);
      logger.error(`handleMetricFreeMint: ${JSON.stringify(error)}`);
    }
  }
}

export default new MetricCommon(
  new PrismaService(),
  new GetCollectionMarketData(),
);
