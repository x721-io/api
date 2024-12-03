import { PartialType } from '@nestjs/swagger';
import { CreatePlatformTemplateDto } from './create-platform-template.dto';

export class UpdatePlatformTemplateDto extends PartialType(CreatePlatformTemplateDto) {}
