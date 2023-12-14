import { ProjectRound, RoundInfo } from '@prisma/client';

export class RoundEntity implements ProjectRound {
  projectId: string;
  roundId: number;
  address: string;
  id: number;
  name: string;
  description: string;
  start: Date;
  end: Date;
}
