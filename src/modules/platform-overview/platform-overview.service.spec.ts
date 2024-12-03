import { Test, TestingModule } from '@nestjs/testing';
import { PlatformOverviewService } from './platform-overview.service';

describe('PlatformOverviewService', () => {
  let service: PlatformOverviewService;

  beforeEach(async () => {
    const module: TestingModule = await Test.createTestingModule({
      providers: [PlatformOverviewService],
    }).compile();

    service = module.get<PlatformOverviewService>(PlatformOverviewService);
  });

  it('should be defined', () => {
    expect(service).toBeDefined();
  });
});
