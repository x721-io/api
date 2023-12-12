import { PartialType } from '@nestjs/swagger';
import { CreateLaunchpadDto } from './create-launchpad.dto';

export class UpdateLaunchpadDto extends PartialType(CreateLaunchpadDto) {}
