import { Trait } from '@prisma/client';

export class TraitEntity implements Trait {
  collectionId: string;
  trait_type: string;
  display_type: string;
  value: string;
  nftId: string;
  id: string;
}
