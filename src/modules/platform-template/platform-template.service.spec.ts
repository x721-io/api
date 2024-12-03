import { Test, TestingModule } from '@nestjs/testing';
import { PlatformTemplateService } from './platform-template.service';

describe('PlatformTemplateService', () => {
  let service: PlatformTemplateService;

  beforeEach(async () => {
    const module: TestingModule = await Test.createTestingModule({
      providers: [PlatformTemplateService],
    }).compile();

    service = module.get<PlatformTemplateService>(PlatformTemplateService);
  });

  it('should be defined', () => {
    expect(service).toBeDefined();
  });
});
