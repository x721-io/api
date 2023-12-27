import { ProjectRound } from '@prisma/client';

export class RoundEntity implements ProjectRound {
  price: string;
  maxPerWallet: number;
  totalNftt: number;
  claimableStart: Date;
  claimableIds: string[];
  projectId: string;
  roundId: number;
  address: string;
  id: number;
  name: string;
  description: string;
  start: Date;
  end: Date;
}
