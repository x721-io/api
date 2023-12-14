import { Test, TestingModule } from '@nestjs/testing';
import { LaunchpadService } from './launchpad.service';

describe('LaunchpadService', () => {
  let service: LaunchpadService;

  beforeEach(async () => {
    const module: TestingModule = await Test.createTestingModule({
      providers: [LaunchpadService],
    }).compile();

    service = module.get<LaunchpadService>(LaunchpadService);
  });

  it('should be defined', () => {
    expect(service).toBeDefined();
  });
});
