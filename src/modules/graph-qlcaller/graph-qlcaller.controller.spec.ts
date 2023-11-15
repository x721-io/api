import { Test, TestingModule } from '@nestjs/testing';
import { GraphQlcallerController } from './graph-qlcaller.controller';
import { GraphQlcallerService } from './graph-qlcaller.service';

describe('GraphQlcallerController', () => {
  let controller: GraphQlcallerController;

  beforeEach(async () => {
    const module: TestingModule = await Test.createTestingModule({
      controllers: [GraphQlcallerController],
      providers: [GraphQlcallerService],
    }).compile();

    controller = module.get<GraphQlcallerController>(GraphQlcallerController);
  });

  it('should be defined', () => {
    expect(controller).toBeDefined();
  });
});
