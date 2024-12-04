import { PartialType } from '@nestjs/swagger';
import { CreatePlatformOverviewDto } from './create-platform-overview.dto';

export class UpdatePlatformOverviewDto extends PartialType(
  CreatePlatformOverviewDto,
) {}
