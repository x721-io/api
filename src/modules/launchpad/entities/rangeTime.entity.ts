import { RoundRangeTime } from '@prisma/client';

export class RoundRangeTimeEntity implements RoundRangeTime {
  id: string;
  projectId: string;
  roundId: number;
  start: Date;
  end: Date;
}
