import { Test, TestingModule } from '@nestjs/testing';
import { PlatformOverviewController } from './platform-overview.controller';
import { PlatformOverviewService } from './platform-overview.service';

describe('PlatformOverviewController', () => {
  let controller: PlatformOverviewController;

  beforeEach(async () => {
    const module: TestingModule = await Test.createTestingModule({
      controllers: [PlatformOverviewController],
      providers: [PlatformOverviewService],
    }).compile();

    controller = module.get<PlatformOverviewController>(PlatformOverviewController);
  });

  it('should be defined', () => {
    expect(controller).toBeDefined();
  });
});
