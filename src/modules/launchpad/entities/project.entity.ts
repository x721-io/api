import { Project } from '@prisma/client';
import { RoundEntity } from './round.entity';

export class ProjectEntity implements Project {
  isActivated: boolean;
  id: string;
  idOnchain: number;
  name: string;
  banner: string;
  description: string;
  organization: string;
  website: string;
  teleLink: string;
  facebookLink: string;
  instaLink: string;
  discordLink: string;
  shortLink: string;
  rounds: RoundEntity[];
}
