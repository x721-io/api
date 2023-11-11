import { Test, TestingModule } from '@nestjs/testing';
import { ApiCallerService } from './api-caller.service';

describe('ApiCallerService', () => {
  let service: ApiCallerService;

  beforeEach(async () => {
    const module: TestingModule = await Test.createTestingModule({
      providers: [ApiCallerService],
    }).compile();

    service = module.get<ApiCallerService>(ApiCallerService);
  });

  it('should be defined', () => {
    expect(service).toBeDefined();
  });
});
