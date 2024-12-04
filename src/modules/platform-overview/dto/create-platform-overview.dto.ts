import { OffsetPaginationDto } from '../../../commons/definitions/OffsetPagination.input';

export class CreatePlatformOverviewDto {
  platform: string;
  name: string;
  avatar?: string;
  banner?: string;
  description?: string;
}

export class PlatformOverviewFilter extends OffsetPaginationDto{
  platform?: string;
  templateStatus?: string;
}
