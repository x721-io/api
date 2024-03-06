// async findAll(filter: GetAllNftDto): Promise<PagingResponseHasNext<NftDto>> {
//   // TODO: Reimplement pagination strategy
//   // Get the result totally from subgraph and match data to local storage
//   // For each set of condition, use different subgraph query as source
//   // owner: getNFTFromOwner
//   // owner + sellStatus + priceMax + priceMin + collectionType: marketplace721S and marketplace1155S
//   try {
//     let traitsConditions = [];

//     // TODO: if price and status are included, then use subgraph as main source and use other to eliminate
//     if (filter.traits) {
//       traitsConditions = filter.traits.map((trait) => ({
//         traits: {
//           some: {
//             trait_type: trait.trait_type,
//             ...(trait.value && { value: trait.value }),
//             ...(trait.display_type && { display_type: trait.display_type }),
//           },
//         },
//       }));
//     }
//     let nftIdFromOwner = [];
//     let nftCollectionFromOwner = [];
//     let hasNextNftOwner = false;
//     if (filter.owner) {
//       const { account } = await this.GraphqlService.getNFTFromOwner(
//         filter.owner.toLocaleLowerCase(),
//         filter.order as OrderDirection,
//         filter.page,
//         Math.floor(filter.limit / 2),
//       );
//       const { account: hasNextNftOwnerTemp } =
//         await this.GraphqlService.getNFTFromOwner(
//           filter.owner.toLocaleLowerCase(),
//           filter.order as OrderDirection,
//           filter.page + 1,
//           Math.floor(filter.limit / 2),
//         );
//       hasNextNftOwner =
//         hasNextNftOwnerTemp.ERC721tokens.length > 0 ||
//         hasNextNftOwnerTemp.ERC1155balances.length > 0;
//       // console.log(account);
//       if (account) {
//         const erc1155BalancesSort = this.sortERC1155balances(
//           account.ERC1155balances,
//           filter.order,
//         );
//         nftIdFromOwner = account.ERC721tokens.map(
//           (item) => item.tokenId,
//         ).concat(erc1155BalancesSort.map((item) => item.token.tokenId));

//         nftCollectionFromOwner = account.ERC721tokens.map(
//           (item) => item.contract.id,
//         ).concat(erc1155BalancesSort.map((item) => item.token.contract.id));
//       }
//     }

//     const whereCondition: Prisma.NFTWhereInput = {};
//     const whereConditionInternal: Prisma.NFTWhereInput = {};
//     whereConditionInternal.AND = [];
//     whereCondition.OR = [];

//     // Handle traits conditions
//     if (traitsConditions.length > 0) {
//       whereConditionInternal.AND.push(...traitsConditions);
//     }

//     whereConditionInternal.AND.push({
//       status: TX_STATUS.SUCCESS,
//     });

//     if (filter.creatorAddress) {
//       whereConditionInternal.AND.push({
//         creator: {
//           publicKey: filter.creatorAddress,
//         },
//       });
//     }

//     if (filter.collectionAddress || filter.type) {
//       const collectionCondition: Prisma.CollectionWhereInput = {};

//       if (filter.collectionAddress) {
//         collectionCondition.address = filter.collectionAddress;
//       }

//       if (filter.type) {
//         collectionCondition.type = filter.type;
//       }

//       whereConditionInternal.AND.push({ collection: collectionCondition });
//     }

//     if (filter.name) {
//       whereConditionInternal.AND.push({
//         // name: {
//         //   contains: filter.name,
//         //   mode: 'insensitive',
//         // },
//         nameSlug: {
//           contains: OtherCommon.stringToSlugSearch(filter.name),
//           mode: 'insensitive',
//         },
//       });
//     }

//     if (nftIdFromOwner.length > 0) {
//       const collectionToTokenIds: Record<string, string[]> = {};
//       for (let i = 0; i < nftIdFromOwner.length; i++) {
//         const collection = nftCollectionFromOwner[i];
//         if (!collectionToTokenIds[collection]) {
//           collectionToTokenIds[collection] = [];
//         }
//         collectionToTokenIds[collection].push(nftIdFromOwner[i]);
//       }
//       for (const [collection, tokenIds] of Object.entries(
//         collectionToTokenIds,
//       )) {
//         const tokenIdConditions = tokenIds.map((tokenId) => ({
//           OR: [{ u2uId: tokenId }, { id: tokenId }],
//         }));
//         whereCondition.OR.push({
//           AND: [
//             { OR: tokenIdConditions },
//             {
//               collection: {
//                 address: collection,
//               },
//             },
//             ...whereConditionInternal.AND,
//           ],
//         });
//       }
//     } else if (filter.owner) {
//       // console.log(whereConditionInternal);
//     } else {
//       whereCondition.AND = whereConditionInternal.AND;
//       delete whereCondition.OR;
//     }

//     //----------

//     if (
//       (!filter.priceMin && !filter.priceMax && !filter.sellStatus) ||
//       filter.name
//     ) {
//       if (filter.quoteToken !== undefined) {
//         whereCondition.MarketplaceByTokenId = { some: {} };
//         whereCondition.MarketplaceByTokenId.some.quoteToken =
//           filter.quoteToken;
//       }
//       const whereMarketPlaceStatus: Prisma.MarketplaceStatusWhereInput =
//         this.generateWhereMarketPlaceStatus(filter);

//       const nfts = await this.prisma.nFT.findMany({
//         ...(!filter.owner && {
//           skip: (filter.page - 1) * filter.limit,
//           take: filter.limit,
//         }),
//         // take: filter.limit,
//         // where: whereCondition.OR.length > 0 || whereConditionInternal.AND.length > 0 ? whereCondition : { AND: [] },
//         where: whereCondition,
//         orderBy: {
//           createdAt: filter.order,
//         },
//         include: {
//           creator: {
//             select: creatorSelect,
//           },
//           collection: {
//             select: CollectionSelect,
//           },
//           MarketplaceByTokenId: {
//             where: whereMarketPlaceStatus,
//             select: marketplaceSelect,
//           },
//           traits: true,
//         },
//       });
//       const Nftformat = nfts.map((item) => {
//         if (
//           item?.MarketplaceByTokenId &&
//           item?.MarketplaceByTokenId.length > 0
//         ) {
//           const { priceWei, event, quantity, askId, quoteToken } =
//             item.MarketplaceByTokenId.reduce(
//               (minItem, currentItem) =>
//                 currentItem.price < minItem.price ? currentItem : minItem,
//               item.MarketplaceByTokenId[0],
//             );
//           delete item.MarketplaceByTokenId;
//           return {
//             ...item,
//             price: priceWei,
//             sellStatus: event,
//             quantity,
//             askId,
//             quoteToken,
//           };
//         } else {
//           delete item.MarketplaceByTokenId;
//           return item;
//         }
//       });

//       const hasNext =
//         (await PaginationCommon.hasNextPage(
//           filter.page,
//           filter.limit,
//           'nFT',
//           whereCondition,
//         )) || hasNextNftOwner;
//       return {
//         data: Nftformat,
//         paging: {
//           hasNext: hasNext,
//           limit: filter.limit,
//           page: filter.page,
//         },
//       };
//     } else {
//       if (Number(filter.priceMin) > Number(filter.priceMax)) {
//         // If priceMin is higher than priceMax, return an empty array
//         return {
//           data: [],
//           paging: {
//             hasNext: false,
//             limit: filter.limit,
//             page: filter.page,
//           },
//         };
//       }
//       const whereMarketPlaceStatus: Prisma.MarketplaceStatusWhereInput =
//         this.generateWhereMarketPlaceStatus(filter);
//       const whereCondition1: Prisma.NFTWhereInput = {
//         AND: [whereCondition],
//       };
//       // Ensure that MarketplaceByTokenId is initialized
//       if (!whereCondition1.MarketplaceByTokenId) {
//         whereCondition1.MarketplaceByTokenId = { some: {} };
//       }

//       if (filter.priceMin !== undefined || filter.priceMax !== undefined) {
//         whereCondition1.MarketplaceByTokenId.some.price = {};
//         if (filter.priceMin !== undefined) {
//           whereCondition1.MarketplaceByTokenId.some.price.gte = Number(
//             OtherCommon.weiToEther(filter.priceMin),
//           );
//         }
//         if (filter.priceMax !== undefined) {
//           whereCondition1.MarketplaceByTokenId.some.price.lte = Number(
//             OtherCommon.weiToEther(filter.priceMax),
//           );
//         }
//       }
//       // Check if filter.from or filter.quoteToken is defined before adding it to the query
//       if (filter.from !== undefined || filter.owner !== undefined) {
//         whereCondition1.MarketplaceByTokenId.some.from =
//           filter.sellStatus === SellStatus.AskNew && filter.owner
//             ? filter.owner.toLowerCase()
//             : filter.from;
//       }

//       if (filter.quoteToken !== undefined) {
//         whereCondition1.MarketplaceByTokenId.some.quoteToken =
//           filter.quoteToken;
//       }
//       const nfts = await this.prisma.nFT.findMany({
//         // skip: (filter.page - 1) * filter.limit,
//         // take: filter.limit,
//         where: whereCondition1,
//         orderBy: {
//           createdAt: filter.order,
//         },
//         include: {
//           creator: {
//             select: creatorSelect,
//           },
//           collection: {
//             select: CollectionSelect,
//           },
//           MarketplaceByTokenId: {
//             where: whereMarketPlaceStatus,
//             select: marketplaceSelect,
//           },
//           traits: true,
//         },
//       });
//       const Nftformat = nfts.map((item) => {
//         const { priceWei, event, quantity, askId, quoteToken } =
//           item.MarketplaceByTokenId.reduce(
//             (minItem, currentItem) =>
//               currentItem.price < minItem.price ? currentItem : minItem,
//             item.MarketplaceByTokenId[0],
//           );
//         delete item.MarketplaceByTokenId;
//         return {
//           ...item,
//           price: priceWei,
//           sellStatus: event,
//           quantity,
//           askId,
//           quoteToken,
//         };
//       });
//       const hasNext = await PaginationCommon.hasNextPage(
//         filter.page,
//         filter.limit,
//         'nFT',
//         whereCondition1,
//       );
//       return {
//         data: Nftformat,
//         paging: {
//           hasNext: hasNext,
//           limit: filter.limit,
//           page: filter.page,
//         },
//       };
//     }
//   } catch (error) {
//     console.error(error);
//     throw new HttpException(`${error.message}`, HttpStatus.BAD_REQUEST);
//   }
// }

// async findAll(filter: GetAllNftDto): Promise<PagingResponseHasNext<NftDto>> {
//   // TODO: Reimplement pagination strategy
//   // Get the result totally from subgraph and match data to local storage
//   // For each set of condition, use different subgraph query as source
//   // owner: getNFTFromOwner
//   // owner + sellStatus + priceMax + priceMin + collectionType: marketplace721S and marketplace1155S
//   try {
//     let traitsConditions = [];

//     // TODO: if price and status are included, then use subgraph as main source and use other to eliminate
//     if (filter.traits) {
//       traitsConditions = filter.traits.map((trait) => ({
//         traits: {
//           some: {
//             trait_type: trait.trait_type,
//             ...(trait.value && { value: trait.value }),
//             ...(trait.display_type && { display_type: trait.display_type }),
//           },
//         },
//       }));
//     }
//     let nftIdFromOwner = [];
//     let nftCollectionFromOwner = [];
//     let hasNextNftOwner = false;
//     if (filter.owner) {
//       const { account } = await this.GraphqlService.getNFTFromOwner(
//         filter.owner.toLocaleLowerCase(),
//         filter.order as OrderDirection,
//         filter.page,
//         Math.floor(filter.limit / 2),
//       );
//       const { account: hasNextNftOwnerTemp } =
//         await this.GraphqlService.getNFTFromOwner(
//           filter.owner.toLocaleLowerCase(),
//           filter.order as OrderDirection,
//           filter.page + 1,
//           Math.floor(filter.limit / 2),
//         );
//       hasNextNftOwner =
//         hasNextNftOwnerTemp.ERC721tokens.length > 0 ||
//         hasNextNftOwnerTemp.ERC1155balances.length > 0;
//       // console.log(account);
//       if (account) {
//         const erc1155BalancesSort = this.sortERC1155balances(
//           account.ERC1155balances,
//           filter.order,
//         );
//         nftIdFromOwner = account.ERC721tokens.map(
//           (item) => item.tokenId,
//         ).concat(erc1155BalancesSort.map((item) => item.token.tokenId));

//         nftCollectionFromOwner = account.ERC721tokens.map(
//           (item) => item.contract.id,
//         ).concat(erc1155BalancesSort.map((item) => item.token.contract.id));
//       }
//     }

//     const whereCondition: Prisma.NFTWhereInput = {};
//     const whereConditionInternal: Prisma.NFTWhereInput = {};
//     whereConditionInternal.AND = [];
//     whereCondition.OR = [];

//     // Handle traits conditions
//     if (traitsConditions.length > 0) {
//       whereConditionInternal.AND.push(...traitsConditions);
//     }

//     whereConditionInternal.AND.push({
//       status: TX_STATUS.SUCCESS,
//     });

//     if (filter.creatorAddress) {
//       whereConditionInternal.AND.push({
//         creator: {
//           publicKey: filter.creatorAddress,
//         },
//       });
//     }

//     if (filter.collectionAddress || filter.type) {
//       const collectionCondition: Prisma.CollectionWhereInput = {};

//       if (filter.collectionAddress) {
//         collectionCondition.address = filter.collectionAddress;
//       }

//       if (filter.type) {
//         collectionCondition.type = filter.type;
//       }

//       whereConditionInternal.AND.push({ collection: collectionCondition });
//     }

//     if (filter.name) {
//       whereConditionInternal.AND.push({
//         // name: {
//         //   contains: filter.name,
//         //   mode: 'insensitive',
//         // },
//         nameSlug: {
//           contains: OtherCommon.stringToSlugSearch(filter.name),
//           mode: 'insensitive',
//         },
//       });
//     }

//     if (nftIdFromOwner.length > 0) {
//       const collectionToTokenIds: Record<string, string[]> = {};
//       for (let i = 0; i < nftIdFromOwner.length; i++) {
//         const collection = nftCollectionFromOwner[i];
//         if (!collectionToTokenIds[collection]) {
//           collectionToTokenIds[collection] = [];
//         }
//         collectionToTokenIds[collection].push(nftIdFromOwner[i]);
//       }
//       for (const [collection, tokenIds] of Object.entries(
//         collectionToTokenIds,
//       )) {
//         const tokenIdConditions = tokenIds.map((tokenId) => ({
//           OR: [{ u2uId: tokenId }, { id: tokenId }],
//         }));
//         whereCondition.OR.push({
//           AND: [
//             { OR: tokenIdConditions },
//             {
//               collection: {
//                 address: collection,
//               },
//             },
//             ...whereConditionInternal.AND,
//           ],
//         });
//       }
//     } else if (filter.owner) {
//       // console.log(whereConditionInternal);
//     } else {
//       whereCondition.AND = whereConditionInternal.AND;
//       delete whereCondition.OR;
//     }

//     //----------

//     if (
//       (!filter.priceMin && !filter.priceMax && !filter.sellStatus) ||
//       filter.name
//     ) {
//       const nfts = await this.prisma.nFT.findMany({
//         ...(!filter.owner && {
//           skip: (filter.page - 1) * filter.limit,
//           take: filter.limit,
//         }),
//         // take: filter.limit,
//         // where: whereCondition.OR.length > 0 || whereConditionInternal.AND.length > 0 ? whereCondition : { AND: [] },
//         where: whereCondition,
//         orderBy: {
//           createdAt: filter.order,
//         },
//         include: {
//           creator: {
//             select: creatorSelect,
//           },
//           collection: {
//             select: CollectionSelect,
//           },
//           traits: true,
//         },
//       });
//       const mergedArray = await Promise.all(
//         nfts.map(async (item) => {
//           const { marketEvent1155S, marketEvent721S } =
//             await this.GraphqlService.getNFTSellStatus1({
//               and: [
//                 {
//                   address: item.collection.address,
//                   nftId_: {
//                     tokenId: item.u2uId || item.id,
//                   },
//                 },
//               ],
//             });
//           const foundItem1 = marketEvent721S.find(
//             (obj) =>
//               obj.nftId &&
//               (obj.nftId.tokenId === item.u2uId ||
//                 obj.nftId.tokenId === item.id) &&
//               obj.nftId.contract.id === item.collection.address,
//           );
//           const foundItem2 = marketEvent1155S.find(
//             (obj) =>
//               obj.nftId &&
//               (obj.nftId.tokenId === item.u2uId ||
//                 obj.nftId.tokenId === item.id) &&
//               obj.nftId.contract.id === item.collection.address,
//           );
//           return {
//             ...item,
//             ...(foundItem1 && {
//               price: foundItem1.price,
//               sellStatus: foundItem1.event,
//               quantity: 1,
//               quoteToken: foundItem1.quoteToken,
//             }),
//             ...(foundItem2 && {
//               price: foundItem2.price,
//               sellStatus: foundItem2.event,
//               quantity: foundItem2.quantity,
//               askId: foundItem2.id,
//               quoteToken: foundItem2.quoteToken,
//             }),
//           };
//         }),
//       );
//       const hasNext =
//         (await PaginationCommon.hasNextPage(
//           filter.page,
//           filter.limit,
//           'nFT',
//           whereCondition,
//         )) || hasNextNftOwner;
//       return {
//         data: mergedArray,
//         paging: {
//           hasNext: hasNext,
//           limit: filter.limit,
//           page: filter.page,
//         },
//       };
//     } else {
//       if (Number(filter.priceMin) > Number(filter.priceMax)) {
//         // If priceMin is higher than priceMax, return an empty array
//         return {
//           data: [],
//           paging: {
//             hasNext: false,
//             limit: filter.limit,
//             page: filter.page,
//           },
//         };
//       }
//       const { marketEvent1155S, marketEvent721S, hasNextSubGraph } =
//         await this.GraphqlService.getNFTSellStatus1(
//           {
//             and: [
//               { price_gte: filter.priceMin },
//               { price_lte: filter.priceMax },
//               { event: filter.sellStatus },
//               { quoteToken: filter.quoteToken },
//               {
//                 from:
//                   filter.sellStatus === SellStatus.AskNew && filter.owner
//                     ? filter.owner.toLowerCase()
//                     : filter.from,
//               },
//             ],
//           },
//           filter.page,
//           Math.floor(filter.limit / 2),
//         );
//       const marketEvents = marketEvent1155S
//         // @ts-ignore
//         .concat(marketEvent721S)
//         .filter((i) => !!i.nftId)
//         .map((pair) => ({
//           AND: [
//             { collection: { address: pair.nftId.contract.id } },
//             // { OR: u2uId: pair.nftId.tokenId },
//             {
//               OR: [{ u2uId: pair.nftId.tokenId }, { id: pair.nftId.tokenId }],
//             },
//           ],
//         }));

//       const whereCondition1: Prisma.NFTWhereInput =
//         marketEvents.length > 0
//           ? { AND: [{ OR: marketEvents }, whereCondition] }
//           : { AND: [{ id: '' }, whereCondition] };

//       const nfts = await this.prisma.nFT.findMany({
//         // skip: (filter.page - 1) * filter.limit,
//         // take: filter.limit,
//         where: whereCondition1,
//         orderBy: {
//           createdAt: filter.order,
//         },
//         include: {
//           creator: {
//             select: creatorSelect,
//           },
//           collection: {
//             select: CollectionSelect,
//           },
//           traits: true,
//         },
//       });
//       const mergedArray = nfts.map((item) => {
//         const foundItem1 = marketEvent721S.find(
//           (obj) =>
//             obj.nftId &&
//             (obj.nftId.tokenId === item.u2uId ||
//               obj.nftId.tokenId === item.id) &&
//             obj.nftId.contract.id === item.collection.address,
//         );
//         const foundItem2 = marketEvent1155S.find(
//           (obj) =>
//             obj.nftId &&
//             (obj.nftId.tokenId === item.u2uId ||
//               obj.nftId.tokenId === item.id) &&
//             obj.nftId.contract.id === item.collection.address,
//         );
//         return {
//           ...item,
//           ...(foundItem1 && {
//             price: foundItem1.price,
//             sellStatus: foundItem1.event,
//             quantity: 1,
//             quoteToken: foundItem1.quoteToken,
//           }),
//           ...(foundItem2 && {
//             price: foundItem2.price,
//             sellStatus: foundItem2.event,
//             quantity: foundItem2.quantity,
//             askId: foundItem2.id,
//             quoteToken: foundItem2.quoteToken,
//           }),
//         };
//       });
//       // const hasNext = await PaginationCommon.hasNextPage(
//       //   filter.page,
//       //   filter.limit,
//       //   'nFT',
//       //   whereCondition1,
//       // );

//       // console.log(hasNextSubGraph);
//       return {
//         data: mergedArray,
//         paging: {
//           hasNext: hasNextSubGraph,
//           limit: filter.limit,
//           page: filter.page,
//         },
//       };
//     }
//   } catch (error) {
//     console.error(error);
//     throw new HttpException(`${error.message}`, HttpStatus.BAD_REQUEST);
//   }
// }

// async findAll(filter: GetAllNftDto): Promise<PagingResponseHasNext<NftDto>> {
//   // TODO: Reimplement pagination strategy
//   // Get the result totally from subgraph and match data to local storage
//   // For each set of condition, use different subgraph query as source
//   // owner: getNFTFromOwner
//   // owner + sellStatus + priceMax + priceMin + collectionType: marketplace721S and marketplace1155S
//   try {
//     let traitsConditions = [];

//     // TODO: if price and status are included, then use subgraph as main source and use other to eliminate
//     if (filter.traits) {
//       traitsConditions = filter.traits.map((trait) => ({
//         traits: {
//           some: {
//             trait_type: trait.trait_type,
//             ...(trait.value && { value: trait.value }),
//             ...(trait.display_type && { display_type: trait.display_type }),
//           },
//         },
//       }));
//     }
//     let nftIdFromOwner = [];
//     let nftCollectionFromOwner = [];
//     if (filter.owner) {
//       const { account } = await this.GraphqlService.getNFTFromOwner(
//         filter.owner.toLocaleLowerCase(),
//         filter.order as OrderDirection,
//         filter.page,
//         filter.limit,
//       );
//       // console.log(account);
//       if (account) {
//         const erc1155BalancesSort = this.sortERC1155balances(
//           account.ERC1155balances,
//           filter.order,
//         );
//         nftIdFromOwner = account.ERC721tokens.map(
//           (item) => item.tokenId,
//         ).concat(erc1155BalancesSort.map((item) => item.token.tokenId));

//         nftCollectionFromOwner = account.ERC721tokens.map(
//           (item) => item.contract.id,
//         ).concat(erc1155BalancesSort.map((item) => item.token.contract.id));
//       }
//     }

//     const whereCondition: Prisma.NFTWhereInput = {};
//     const whereConditionInternal: Prisma.NFTWhereInput = {};
//     whereConditionInternal.AND = [];
//     whereCondition.OR = [];

//     // Handle traits conditions
//     if (traitsConditions.length > 0) {
//       whereConditionInternal.AND.push(...traitsConditions);
//     }

//     whereConditionInternal.AND.push({
//       status: TX_STATUS.SUCCESS,
//     });

//     if (filter.creatorAddress) {
//       whereConditionInternal.AND.push({
//         creator: {
//           publicKey: filter.creatorAddress,
//         },
//       });
//     }

//     if (filter.collectionAddress || filter.type) {
//       const collectionCondition: Prisma.CollectionWhereInput = {};

//       if (filter.collectionAddress) {
//         collectionCondition.address = filter.collectionAddress;
//       }

//       if (filter.type) {
//         collectionCondition.type = filter.type;
//       }

//       whereConditionInternal.AND.push({ collection: collectionCondition });
//     }

//     if (filter.name) {
//       whereConditionInternal.AND.push({
//         // name: {
//         //   contains: filter.name,
//         //   mode: 'insensitive',
//         // },
//         nameSlug: {
//           contains: OtherCommon.stringToSlugSearch(filter.name),
//           mode: 'insensitive',
//         },
//       });
//     }

//     if (nftIdFromOwner.length > 0) {
//       const collectionToTokenIds: Record<string, string[]> = {};
//       for (let i = 0; i < nftIdFromOwner.length; i++) {
//         const collection = nftCollectionFromOwner[i];
//         if (!collectionToTokenIds[collection]) {
//           collectionToTokenIds[collection] = [];
//         }
//         collectionToTokenIds[collection].push(nftIdFromOwner[i]);

//         // whereCondition.OR.push({
//         //   AND: [
//         //     { OR: [{ u2uId: nftIdFromOwner[i] }, { id: nftIdFromOwner[i] }] },
//         //     {
//         //       collection: {
//         //         address: nftCollectionFromOwner[i],
//         //       },
//         //     },
//         //     ...whereConditionInternal.AND,
//         //   ],
//         // });
//       }
//       for (const [collection, tokenIds] of Object.entries(
//         collectionToTokenIds,
//       )) {
//         const tokenIdConditions = tokenIds.map((tokenId) => ({
//           OR: [{ u2uId: tokenId }, { id: tokenId }],
//         }));

//         whereCondition.OR.push({
//           AND: [
//             { OR: tokenIdConditions },
//             {
//               collection: {
//                 address: collection,
//               },
//             },
//             ...whereConditionInternal.AND,
//           ],
//         });
//       }
//     } else if (filter.owner) {
//       // console.log(whereConditionInternal);
//     } else {
//       whereCondition.AND = whereConditionInternal.AND;
//       delete whereCondition.OR;
//     }

//     //----------

//     if (!filter.priceMin && !filter.priceMax && !filter.sellStatus) {
//       const nfts = await this.prisma.nFT.findMany({
//         skip: (filter.page - 1) * filter.limit,
//         take: filter.limit,
//         // where: whereCondition.OR.length > 0 || whereConditionInternal.AND.length > 0 ? whereCondition : { AND: [] },
//         where: whereCondition,
//         orderBy: {
//           createdAt: filter.order,
//         },
//         include: {
//           creator: {
//             select: creatorSelect,
//           },
//           collection: {
//             select: {
//               id: true,
//               txCreationHash: true,
//               name: true,
//               status: true,
//               type: true,
//               address: true,
//               category: {
//                 select: {
//                   id: true,
//                   name: true,
//                 },
//               },
//             },
//           },
//           traits: true,
//         },
//       });
//       const mergedArray = await Promise.all(
//         nfts.map(async (item) => {
//           const { marketEvent1155S, marketEvent721S } =
//             await this.GraphqlService.getNFTSellStatus1({
//               and: [
//                 {
//                   address: item.collection.address,
//                   nftId_: {
//                     tokenId: item.u2uId || item.id,
//                   },
//                 },
//               ],
//             });
//           const foundItem1 = marketEvent721S.find(
//             (obj) =>
//               obj.nftId &&
//               (obj.nftId.tokenId === item.u2uId ||
//                 obj.nftId.tokenId === item.id) &&
//               obj.nftId.contract.id === item.collection.address,
//           );
//           const foundItem2 = marketEvent1155S.find(
//             (obj) =>
//               obj.nftId &&
//               (obj.nftId.tokenId === item.u2uId ||
//                 obj.nftId.tokenId === item.id) &&
//               obj.nftId.contract.id === item.collection.address,
//           );
//           return {
//             ...item,
//             ...(foundItem1 && {
//               price: foundItem1.price,
//               sellStatus: foundItem1.event,
//               quantity: 1,
//               quoteToken: foundItem1.quoteToken,
//             }),
//             ...(foundItem2 && {
//               price: foundItem2.price,
//               sellStatus: foundItem2.event,
//               quantity: foundItem2.quantity,
//               askId: foundItem2.id,
//               quoteToken: foundItem2.quoteToken,
//             }),
//           };
//         }),
//       );
//       const hasNext = await PaginationCommon.hasNextPage(
//         filter.page,
//         filter.limit,
//         'nFT',
//         whereCondition,
//       );
//       return {
//         data: mergedArray,
//         paging: {
//           hasNext: hasNext,
//           limit: filter.limit,
//           page: filter.page,
//         },
//       };
//     } else {
//       if (Number(filter.priceMin) > Number(filter.priceMax)) {
//         // If priceMin is higher than priceMax, return an empty array
//         return {
//           data: [],
//           paging: {
//             hasNext: false,
//             limit: filter.limit,
//             page: filter.page,
//           },
//         };
//       }
//       const { marketEvent1155S, marketEvent721S, hasNextSubGraph } =
//         await this.GraphqlService.getNFTSellStatus1(
//           {
//             and: [
//               { price_gte: filter.priceMin },
//               { price_lte: filter.priceMax },
//               { event: filter.sellStatus },
//               { quoteToken: filter.quoteToken },
//               {
//                 from:
//                   filter.sellStatus === SellStatus.AskNew && filter.owner
//                     ? filter.owner.toLowerCase()
//                     : filter.from,
//               },
//             ],
//           },
//           filter.page,
//           filter.limit,
//         );
//       const marketEvents = marketEvent1155S
//         // @ts-ignore
//         .concat(marketEvent721S)
//         .filter((i) => !!i.nftId)
//         .map((pair) => ({
//           AND: [
//             { collection: { address: pair.nftId.contract.id } },
//             // { OR: u2uId: pair.nftId.tokenId },
//             {
//               OR: [{ u2uId: pair.nftId.tokenId }, { id: pair.nftId.tokenId }],
//             },
//           ],
//         }));

//       const whereCondition1: Prisma.NFTWhereInput =
//         marketEvents.length > 0
//           ? { AND: [{ OR: marketEvents }, whereCondition] }
//           : { AND: [{ id: '' }, whereCondition] };

//       const nfts = await this.prisma.nFT.findMany({
//         // skip: (filter.page - 1) * filter.limit,
//         // take: filter.limit,
//         where: whereCondition1,
//         orderBy: {
//           createdAt: filter.order,
//         },
//         include: {
//           creator: {
//             select: creatorSelect,
//           },
//           collection: {
//             select: {
//               id: true,
//               txCreationHash: true,
//               name: true,
//               status: true,
//               type: true,
//               address: true,
//               category: {
//                 select: {
//                   id: true,
//                   name: true,
//                 },
//               },
//             },
//           },
//           traits: true,
//         },
//       });
//       const mergedArray = nfts.map((item) => {
//         const foundItem1 = marketEvent721S.find(
//           (obj) =>
//             obj.nftId &&
//             (obj.nftId.tokenId === item.u2uId ||
//               obj.nftId.tokenId === item.id) &&
//             obj.nftId.contract.id === item.collection.address,
//         );
//         const foundItem2 = marketEvent1155S.find(
//           (obj) =>
//             obj.nftId &&
//             (obj.nftId.tokenId === item.u2uId ||
//               obj.nftId.tokenId === item.id) &&
//             obj.nftId.contract.id === item.collection.address,
//         );
//         return {
//           ...item,
//           ...(foundItem1 && {
//             price: foundItem1.price,
//             sellStatus: foundItem1.event,
//             quantity: 1,
//             quoteToken: foundItem1.quoteToken,
//           }),
//           ...(foundItem2 && {
//             price: foundItem2.price,
//             sellStatus: foundItem2.event,
//             quantity: foundItem2.quantity,
//             askId: foundItem2.id,
//             quoteToken: foundItem2.quoteToken,
//           }),
//         };
//       });
//       // const hasNext = await PaginationCommon.hasNextPage(
//       //   filter.page,
//       //   filter.limit,
//       //   'nFT',
//       //   whereCondition1,
//       // );

//       // console.log(hasNextSubGraph);
//       return {
//         data: mergedArray,
//         paging: {
//           hasNext: hasNextSubGraph,
//           limit: filter.limit,
//           page: filter.page,
//         },
//       };
//     }
//   } catch (error) {
//     console.error(error);
//     throw new HttpException(`${error.message}`, HttpStatus.BAD_REQUEST);
//   }
// }
