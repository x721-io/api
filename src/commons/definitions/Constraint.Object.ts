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

export { creatorSelect };
