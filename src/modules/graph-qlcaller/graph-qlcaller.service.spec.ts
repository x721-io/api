import { Test, TestingModule } from '@nestjs/testing';
import { GraphQlcallerService } from './graph-qlcaller.service';

describe('GraphQlcallerService', () => {
  let service: GraphQlcallerService;

  beforeEach(async () => {
    const module: TestingModule = await Test.createTestingModule({
      providers: [GraphQlcallerService],
    }).compile();

    service = module.get<GraphQlcallerService>(GraphQlcallerService);
  });

  it('should be defined', () => {
    expect(service).toBeDefined();
  });
});
