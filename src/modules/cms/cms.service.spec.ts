import { Test, TestingModule } from '@nestjs/testing';
import { CMSService } from './service/cms.service';

describe('AccountService', () => {
  let service: CMSService;

  beforeEach(async () => {
    const module: TestingModule = await Test.createTestingModule({
      providers: [CMSService],
    }).compile();

    service = module.get<CMSService>(CMSService);
  });

  it('should be defined', () => {
    expect(service).toBeDefined();
  });
});
