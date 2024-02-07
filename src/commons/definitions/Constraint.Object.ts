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

export { creatorSelect, CollectionSelect };
