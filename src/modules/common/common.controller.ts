import { Controller, Get, Post, Body, Patch, Param, Delete, UseInterceptors, UploadedFile } from '@nestjs/common';
import { CommonService } from './common.service';
import { CreateCommonDto } from './dto/create-common.dto';
import { UpdateCommonDto } from './dto/update-common.dto';
import { FileInterceptor } from '@nestjs/platform-express';
import { CreateFileDto } from './dto/create-file.dto';

@Controller('common')
export class CommonController {
  constructor(private readonly commonService: CommonService) {}

  @Post('upload-ipfs')
  @UseInterceptors(FileInterceptor('file'))
  uploadIpfs(@UploadedFile() files: Express.Multer.File[], @Body() createFileDto: CreateFileDto) {
    return this.commonService.uploadIpfs(files, createFileDto.metadata);
  }

  @Get()
  findAll() {
    return this.commonService.findAll();
  }

  @Get(':id')
  findOne(@Param('id') id: string) {
    return this.commonService.findOne(+id);
  }

  @Patch(':id')
  update(@Param('id') id: string, @Body() updateCommonDto: UpdateCommonDto) {
    return this.commonService.update(+id, updateCommonDto);
  }

  @Delete(':id')
  remove(@Param('id') id: string) {
    return this.commonService.remove(+id);
  }
}
