import { User } from '@prisma/client';

export type OwnerOutputDto = Pick<
  User,
  'avatar' | 'email' | 'publicKey' | 'signer' | 'username'
> & {
  quantity?: number | 1;
};
