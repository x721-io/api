import { Test, TestingModule } from '@nestjs/testing';
import { PlatformTemplateController } from './platform-template.controller';
import { PlatformTemplateService } from './platform-template.service';

describe('PlatformTemplateController', () => {
  let controller: PlatformTemplateController;

  beforeEach(async () => {
    const module: TestingModule = await Test.createTestingModule({
      controllers: [PlatformTemplateController],
      providers: [PlatformTemplateService],
    }).compile();

    controller = module.get<PlatformTemplateController>(PlatformTemplateController);
  });

  it('should be defined', () => {
    expect(controller).toBeDefined();
  });
});
