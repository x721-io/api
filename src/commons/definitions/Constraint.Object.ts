import { Prisma, User, Collection } from '@prisma/client';
export const creatorSelect: Prisma.UserSelect = {
  id: true,
  email: true,
  avatar: true,
  username: true,
  publicKey: true,
  accountStatus: true,
  verifyEmail: true,
  signer: true,
};

export const CollectionSelect: Prisma.CollectionSelect = {
  id: true,
  txCreationHash: true,
  name: true,
  status: true,
  type: true,
  address: true,
  isVerified: true,
  metadata: true,
  shortUrl: true,
  symbol: true,
  description: true,
  categoryId: true,
  createdAt: true,
  avatar: true,
  isActive: true,
  isU2U: true,
  metadataJson: true,
  source: true,
  category: {
    select: {
      id: true,
      name: true,
    },
  },
};

export const marketplaceSelect: Prisma.MarketplaceStatusSelect = {
  price: true,
  priceWei: true,
  netPrice: true,
  netPriceWei: true,
  quantity: true,
  quoteToken: true,
  operationId: true,
  operation: true,
  askId: true,
  event: true,
};

export const nftSelect: Prisma.NFTSelect = {
  id: true,
  u2uId: true,
  description: true,
  name: true,
  nameSlug: true,
  image: true,
  animationUrl: true,
  createdAt: true,
  updatedAt: true,
  status: true,
  tokenUri: true,
  txCreationHash: true,
  collectionId: true,
  creatorId: true,
  isActive: true,
  creator: {
    select: creatorSelect,
  },
  collection: {
    select: CollectionSelect,
  },
  traits: true,
};

export const nftSelectOwner: Prisma.NFTSelect = {
  id: true,
  name: true,
  traits: true,
  createdAt: true,
  updatedAt: true,
  status: true,
  tokenUri: true,
  txCreationHash: true,
  creator: {
    select: creatorSelect,
  },
  collection: {
    select: CollectionSelect,
  },
};

export const nftOwnerShip: Prisma.UserNFTSelect = {
  quantity: true,
  nft: {
    select: nftSelectOwner,
  },
};

export const userSelect: Prisma.UserSelect = {
  id: true,
  email: true,
  avatar: true,
  username: true,
  publicKey: true,
  accountStatus: true,
  verifyEmail: true,
  signer: true,
};

export const userSelectFull = (currentUserId: string): Prisma.UserSelect => {
  return {
    id: true,
    email: true,
    avatar: true,
    username: true,
    signature: true,
    signedMessage: true,
    signDate: true,
    signer: true,
    publicKey: true,
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
    accountStatus: true,
    verifyEmail: true,
    isActive: true,
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
  };
};

export const projectSelect: Prisma.ProjectSelect = {
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
};

export const collectionSelect: Prisma.CollectionSelect = {
  id: true,
  txCreationHash: true,
  name: true,
  address: true,
  metadata: true,
  shortUrl: true,
  symbol: true,
  description: true,
  status: true,
  type: true,
  categoryId: true,
  createdAt: true,
  avatar: true,
  coverImage: true,
  updatedAt: true,
  projectId: true,
  nameSlug: true,
  isU2U: true,
  isVerified: true,
  floorPrice: true,
  floor: true,
  floorWei: true,
  isActive: true,
  flagExtend: true,
  category: {
    select: {
      id: true,
      name: true,
    },
  },
};

export const accountListSelect: Prisma.AccountSelect = {
  id: true,
  avatar: true,
  fullName: true,
  email: true,
  username: true,
  createdAt: true,
  updatedAt: true,
  twitterLink: true,
  telegramLink: true,
  phone: true,
  roles: true,
  isActive: true,
  isDelete: true,
};

export const userFollow: Prisma.UserSelect = {
  id: true,
  email: true,
  avatar: true,
  username: true,
  publicKey: true,
  accountStatus: true,
  verifyEmail: true,
  signer: true,
  shortLink: true,
};

export const orderSelect: Prisma.OrderSelect = {
  price: true,
  priceNum: true,
  netPrice: true,
  netPriceNum: true,
  quantity: true,
  quoteToken: true,
  orderStatus: true,
  orderType: true,
  index: true,
  sig: true,
  filledQty: true,
  start: true,
  end: true,
};

export const orderNFTSelect: Prisma.OrderSelect = {
  price: true,
  quantity: true,
  quoteToken: true,
  orderStatus: true,
  orderType: true,
  index: true,
  sig: true,
  start: true,
  end: true,
  Taker: {
    select: userSelect,
  },
  Maker: {
    select: userSelect,
  },
};
