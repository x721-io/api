import { Collection, TX_STATUS, CONTRACT_TYPE } from '@prisma/client';
import { TraitGeneralInfo, TraitService } from '../../nft/trait.service';

export class CollectionDetailDto {
  collection: Collection;
  traitAvailable: TraitGeneralInfo[];
  generalInfo: any;
}
