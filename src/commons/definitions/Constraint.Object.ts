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
  metadata: true,
  shortUrl: true,
  symbol: true,
  description: true,
  categoryId: true,
  createdAt: true,
  avatar: true,
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

export { creatorSelect, CollectionSelect, marketplaceSelect };
