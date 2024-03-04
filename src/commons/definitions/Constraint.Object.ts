import { Prisma, User } from '@prisma/client';

const creatorSelect: Prisma.UserSelect = {
  id: true,
  email: true,
  avatar: true,
  username: true,
  publicKey: true,
  accountStatus: true,
  verifyEmail: true,
  signer: true,
};

const CollectionSelect: Prisma.CollectionSelect = {
  id: true,
  txCreationHash: true,
  name: true,
  status: true,
  type: true,
  address: true,
  isVerified: true,
  category: {
    select: {
      id: true,
      name: true,
    },
  },
};

const marketplaceSelect: Prisma.MarketplaceStatusSelect = {
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

const nftSelect: Prisma.NFTSelect = {
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
  creator: {
    select: creatorSelect,
  },
  collection: {
    select: CollectionSelect,
  },
  traits: true,
};

export { creatorSelect, CollectionSelect, marketplaceSelect, nftSelect };
