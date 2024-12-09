import { User } from '@prisma/client';

export type OwnerOutputDto = Partial<
  Pick<User, 'avatar' | 'email' | 'publicKey' | 'signer' | 'username'>
> & {
  quantity?: string | number;
};
