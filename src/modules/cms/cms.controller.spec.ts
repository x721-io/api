import { Test, TestingModule } from '@nestjs/testing';
import { CMSController } from './controller/cms.controller';
import { CMSService } from './service/cms.service';

describe('CMSController', () => {
  let controller: CMSController;

  beforeEach(async () => {
    const module: TestingModule = await Test.createTestingModule({
      controllers: [CMSController],
      providers: [CMSService],
    }).compile();

    controller = module.get<CMSController>(CMSController);
  });

  it('should be defined', () => {
    expect(controller).toBeDefined();
  });
});
