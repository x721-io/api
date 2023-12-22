import { ProjectRound } from '@prisma/client';
import { CollectionEntity } from 'src/modules/collection/entities/collection.entity';

export class RoundEntity implements ProjectRound {
  price: string;
  maxPerWallet: number;
  totalNftt: number;
  claimableStart: Date;
  projectId: string;
  roundId: number;
  address: string;
  id: number;
  name: string;
  description: string;
  start: Date;
  end: Date;
}
