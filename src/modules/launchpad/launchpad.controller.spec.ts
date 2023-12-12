import { Test, TestingModule } from '@nestjs/testing';
import { LaunchpadController } from './launchpad.controller';
import { LaunchpadService } from './launchpad.service';

describe('LaunchpadController', () => {
  let controller: LaunchpadController;

  beforeEach(async () => {
    const module: TestingModule = await Test.createTestingModule({
      controllers: [LaunchpadController],
      providers: [LaunchpadService],
    }).compile();

    controller = module.get<LaunchpadController>(LaunchpadController);
  });

  it('should be defined', () => {
    expect(controller).toBeDefined();
  });
});
