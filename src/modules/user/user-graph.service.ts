import { Injectable, NotFoundException } from '@nestjs/common';
import { CreateUserDto } from './dto/create-user.dto';
import { UpdateUserDto } from './dto/update-user.dto';
import { PrismaService } from 'src/prisma/prisma.service';
import { TX_STATUS } from '@prisma/client';
import { GetAllUser } from './dto/get-all-user.dto';
import { FilterNFTUserDetail } from './dto/get-nft-user.dto';

import { GraphQLClient } from 'graphql-request';
import { getSdk, GetNfTwithAccountIdQueryVariables } from '../../generated/graphql'
import { validate as isValidUUID } from 'uuid'
import { NFTTab } from 'src/constants/enums/NFTTab.enum'
import { SellStatus } from '../../generated/graphql';
import { CollectionService } from '../collection/collection.service';
import { MarketplaceService } from '../nft/nft-marketplace.service';
import { GetEventMarketplace } from '../nft/dto/event-marketplace.dto';
import { CONTRACT_TYPE } from '@prisma/client';
import { NFTIsBid } from '../../constants/enums/NFTTab.enum';
interface NFT {
  id?: string;
  name?: string;
  ipfsHash?: string;
  // traits?: Traits[];
  createdAt?: Date;
  updatedAt?: Date;
  status?: TX_STATUS;
  tokenUri?: string;
  txCreationHash?: string;
  creatorId?: string;
  collectionId?: string
  creator?: USER
}

interface USER {
  id: string;
  publicKey: string;
  email: string;
  avatar?: string;
  username: string;
  signature?: string;
  signedMessage?: string;
  signer?: string;
  acceptedTerms?: boolean;
  // nftsOwnership? : 
}

interface NFTOwnership {
  quantity: number;
  nft?: NFT;
}

interface ERC721Token {
  id: string;
  uri?: string;
  txCreation: string;
}

interface ERC1155Balance {
  id: string;
  valueExact: string;
  value: string;
}


export interface Result {
  // user : USER;
  // ERC721tokens: (ERC721Token)[];
  // ERC1155balances: ERC1155Balance[];
}




@Injectable()
export class UserServiceExtend {
  private readonly prisma: PrismaService;
  private readonly collectionService: CollectionService;
  private readonly marketplaceService: MarketplaceService;

  constructor(prisma: PrismaService, collectionService: CollectionService, marketplaceService: MarketplaceService) {
    this.prisma = prisma;
    this.collectionService = collectionService;
    this.marketplaceService = marketplaceService;
  }

  private readonly endpoint = process.env.SUBGRAPH_URL;

  private client = this.getGraphqlClient();

  private sdk = getSdk(this.client);

  // private readonly allowedEventsOnSales = [SellStatus.AskNew, SellStatus.AskCancel, SellStatus.Trade];

  // private readonly allowedEventsBid = [SellStatus.Bid];

  private getGraphqlClient() {
    return new GraphQLClient(this.endpoint);
  }
  // // Remove few prop secret
  // private minifyUserObjecct(user: any): any {
  //   const propertiesToRemove = ['signature', 'signer', 'signedMessage', 'nftsOwnership', 'nftCreator', 'nftCollection'];
  //   const minifiedUser = { ...user };
  //   for (const property in minifiedUser) {
  //     if (propertiesToRemove.includes(property)) {
  //       delete minifiedUser[property];
  //     }
  //   }
  //   return minifiedUser;
  // }

  async getNFTByUser(id: string, filter: FilterNFTUserDetail): Promise<any> {
    try {
      const { tab, page, limit, has_bid } = filter;
      if (!isValidUUID(id)) {
        throw new Error('Invalid ID. Please try again !');
      }
      const [userData, NFTList] = await Promise.all([
        this.prisma.user.findUnique({
          where: { id },
          include: {
            nftCreator: tab === NFTTab.CREATOR ? {
              select: {
                id: true,
                name: true,
                ipfsHash: true,
                createdAt: true,
                updatedAt: true,
                status: true,
                tokenUri: true,
                txCreationHash: true,
                collectionId: true,
                traits: {
                  select: {
                    id: true,
                    value: true,
                    display_type: true,
                    trait_type: true
                  }
                },
              }
            } : false, 
          }
        }),
        this.prisma.nFT.findMany({
          select: {
            id: true,
            name: true,
            ipfsHash: true,
            createdAt: true,
            updatedAt: true,
            status: true,
            tokenUri: true,
            txCreationHash: true,
            collectionId: true,
            collection : {
              select : {
                id : true,
                txCreationHash : true,
                name : true,
                symbol : true,
                description : true,
                address : true,
                shortUrl : true,
                metadata : true,
                status : true,
                type : true,
                createdAt : true,
                updatedAt : true,
                category : {
                  select : {
                    id : true,
                    name : true,
                  }
                },

              }
            },
            traits: {
              select: {
                id: true,
                value: true,
                display_type: true,
                trait_type: true
              }
            },
          },
        })
      ]);

      if (!userData) {
        throw new NotFoundException()
      }
      const { publicKey } = userData;
      const {ERC721tokens = [], ERC1155balances = [] } = await this.getDataSubgraphNFT(publicKey);
      const { nftCreator = [] } = userData;
      const mergedERC721 = await this.mergeERCData721(ERC721tokens, NFTList, nftCreator, tab, page, limit, has_bid, publicKey);
      const mergedERC1155 = await this.mergeERCData1155(ERC1155balances, NFTList, nftCreator, tab, page, limit, has_bid, publicKey);
      return [...mergedERC721 , ...mergedERC1155]
    } catch (err) {
      console.error(err);
      throw err;
    }
  }

  async getDataSubgraphNFT(publicKey: string): Promise<{ ERC721tokens: any[]; ERC1155balances: any[]}> {
    const { account } = await this.sdk.getNFTwithAccountID({ id: publicKey });
    return { ERC721tokens: account?.ERC721tokens || [], ERC1155balances: account?.ERC1155balances || []};
  }

  async mergeERCData1155(tokens: any[], NFTList: NFT[], nftCreator: any[], tab: NFTTab, page: number, limit: number, has_bid: NFTIsBid | null, publicKey: string | null): Promise<any> {
    // Get data NFT with Tab Creator, On Sales, Owner  
    const mergeNFTAndStatus = async (token: any, event: SellStatus | null) => {
      const tokenId = token?.token?.id || token.id;
      const matchingNFT = NFTList.find(nft => nft.id === tokenId) || {};
  
      const input: GetEventMarketplace = {
        nftId: tokenId,
        page,
        limit,
        type: CONTRACT_TYPE.ERC1155,
        event,
      };
  
      const matchingNFTStatus = await this.marketplaceService.findEvents(input);
  
      return {
        ...token,
        ...matchingNFT,
        events: matchingNFTStatus,
      };
    };

    const getNFTBid = async (event: SellStatus | null) => {
      const input: GetEventMarketplace = {
        page,
        limit,
        type: CONTRACT_TYPE.ERC1155,
        to: publicKey,
        event,
      };
  
      const matchingNFTStatus = await this.marketplaceService.findEvents(input);
  
      return matchingNFTStatus.map(token => {
        const matchingNFT = NFTList.find(nft => nft.id == token?.nftId?.id);
        return {
          ...token,
          ...matchingNFT,
        };
      });
    };

    switch (tab) {
      case NFTTab.CREATOR:
        return Promise.all(nftCreator.map(token => mergeNFTAndStatus(token, null)));
  
      case NFTTab.ON_SALES:
      case NFTTab.OWNER:
        return Promise.all(tokens.map(token => mergeNFTAndStatus(token, tab === NFTTab.ON_SALES ? SellStatus.AskNew : SellStatus.Trade)));
  
      case NFTTab.BID:
        return has_bid === NFTIsBid.TRUE ? getNFTBid(SellStatus.Bid) : Promise.all(tokens.map(token => mergeNFTAndStatus(token, SellStatus.Bid)));
    }
  }

  async mergeERCData721(tokens: any[], NFTList: NFT[], nftCreator: any[], tab: NFTTab, page: number, limit: number, has_bid: NFTIsBid | null, publicKey: string | null): Promise<any> {

    const mergeNFTAndStatus = async (token: any, event: SellStatus | null) => {
      const matchingNFT = NFTList.find(nft => nft.id === token.id) || {};

      const input: GetEventMarketplace = {
        nftId: token.id,
        page,
        limit,
        type: CONTRACT_TYPE.ERC721,
        event,
      };

      const matchingNFTStatus = await this.marketplaceService.findEvents(input);

      return {
        ...token,
        ...matchingNFT,
        events: matchingNFTStatus,
      };
    };

    const getNFTBid = async (event: SellStatus | null) => {
      const input: GetEventMarketplace = {
        page,
        limit,
        type: CONTRACT_TYPE.ERC721,
        to: publicKey,
        event,
      };
  
      const matchingNFTStatus = await this.marketplaceService.findEvents(input);
  
      return matchingNFTStatus.map(token => {
        const matchingNFT = NFTList.find(nft => nft.id == token?.nftId?.id);
        return {
          ...token,
          ...matchingNFT,
        };
      });
    };

    switch (tab) {
      case NFTTab.CREATOR:
        return Promise.all(nftCreator.map(token => mergeNFTAndStatus(token, null)));
  
      case NFTTab.ON_SALES:
      case NFTTab.OWNER:
        return Promise.all(tokens.map(token => mergeNFTAndStatus(token, tab === NFTTab.ON_SALES ? SellStatus.AskNew : null)));
  
      case NFTTab.BID:
        return has_bid === NFTIsBid.TRUE
          ? getNFTBid(SellStatus.Bid)
          : Promise.all(tokens.map(token => mergeNFTAndStatus(token, SellStatus.Bid)));
    }
  }


  async getCollectionByUser(id: string): Promise<any> {
    try {
      if (!isValidUUID(id)) {
        throw new Error('Invalid ID. Please try again !');
      }
      const userData = await this.prisma.user.findUnique({
        where: { id: id },
        include: {
          nftCollection: {
            select: {
              collection: {
                select: {
                  id: true,
                  txCreationHash: true,
                  name: true,
                  symbol: true,
                  description: true,
                  status: true,
                  address: true,
                  type: true,
                  categoryId: true,
                  createdAt: true,
                  category: {
                    select: {
                      id: true,
                      name: true,
                    }
                  }
                }
              }
            }
          }
        }
      })
      if (!userData) {
        throw new NotFoundException();
      }
      let { nftCollection = [] } = userData;
      const subgraphCollection = await Promise.all(nftCollection.map(async (item) => {
        let { collection } = item;
        const generalInfo = await this.collectionService.getGeneralCollectionData(collection.address, collection.type);
        return { ...collection, ...generalInfo }
      }))
      return subgraphCollection
      // return { ...this.minifyUserObjecct(userData), collections : subgraphCollection };
    } catch (err) {
      console.error(err);
      throw err;
    }
  }
}