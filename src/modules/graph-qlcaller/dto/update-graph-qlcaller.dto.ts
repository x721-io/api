import { PartialType } from '@nestjs/mapped-types';
import { CreateGraphQlcallerDto } from './create-graph-qlcaller.dto';

export class UpdateGraphQlcallerDto extends PartialType(CreateGraphQlcallerDto) {}
