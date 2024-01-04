import { SubcribeEntity } from '../../launchpad/entities/subcribe.entity';
import { ProjectEntity } from '../../launchpad/entities/project.entity';

export class ListProjectEntity extends SubcribeEntity {
  project?: ProjectEntity[];
}
