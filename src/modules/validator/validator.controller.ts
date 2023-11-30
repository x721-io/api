import { Controller, Post, Body } from '@nestjs/common';
import { ValidatorDto } from './dto/validator.dto';
import { ValidatorService } from './validator.service';

@Controller('validator')
export class ValidatorController {
  constructor(private readonly validatorService: ValidatorService) {}
  @Post()
  checkExistsKeyValue(@Body() validatorDto: ValidatorDto) {
    return this.validatorService.validateUniqueField(validatorDto);
  }
}
