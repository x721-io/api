import { Prisma, Project } from '@prisma/client';
import { RoundEntity } from './round.entity';
import { CollectionEntity } from 'src/modules/collection/entities/collection.entity';

export class ProjectEntity implements Project {
  details: Prisma.JsonValue[];
  twitter: string;
  logo: string;
  isActivated: boolean;
  id: string;
  idOnchain: number;
  name: string;
  banner: string;
  description: string;
  organization: string;
  website: string;
  telegram: string;
  facebook: string;
  instagram: string;
  discord: string;
  shortLink: string;
  isDelete: boolean;
  rounds: RoundEntity[];
  collection: CollectionEntity;
}
