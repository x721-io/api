import { Controller, Get, Post, Body, Patch, Param, Delete, Query } from '@nestjs/common';
import { GraphQlcallerService } from './graph-qlcaller.service';
import { CreateGraphQlcallerDto } from './dto/create-graph-qlcaller.dto';
import { UpdateGraphQlcallerDto } from './dto/update-graph-qlcaller.dto';

@Controller('graph-qlcaller')
export class GraphQlcallerController {
  constructor(private readonly graphQlcallerService: GraphQlcallerService) {}

  @Post()
  create(@Body() createGraphQlcallerDto: CreateGraphQlcallerDto) {
    return this.graphQlcallerService.create(createGraphQlcallerDto);
  }

  @Get('collection')
  findAllCollection(@Query('first') first: number) {
    return this.graphQlcallerService.getCollections(1);
  }

  @Get(':id')
  findOne(@Param('id') id: string) {
    return this.graphQlcallerService.findOne(+id);
  }

  @Patch(':id')
  update(@Param('id') id: string, @Body() updateGraphQlcallerDto: UpdateGraphQlcallerDto) {
    return this.graphQlcallerService.update(+id, updateGraphQlcallerDto);
  }

  @Delete(':id')
  remove(@Param('id') id: string) {
    return this.graphQlcallerService.remove(+id);
  }
}
