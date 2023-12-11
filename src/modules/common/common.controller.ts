import {
  Controller,
  Get,
  Post,
  Body,
  Patch,
  Param,
  Delete,
  UseInterceptors,
  UploadedFiles,
  Query,
  Res,
} from '@nestjs/common';
import { CommonService } from './common.service';
import { UpdateCommonDto } from './dto/update-common.dto';
import { FilesInterceptor } from '@nestjs/platform-express';
import { CreateFileDto } from './dto/create-file.dto';
import { Response } from 'express';
import { SearchAllDto } from './dto/search-all.dto';
import { multerOptions } from './interceptor/interceptor';
@Controller('common')
export class CommonController {
  constructor(private readonly commonService: CommonService) {}

  @Post('upload-ipfs')
  @UseInterceptors(FilesInterceptor('files', 10, multerOptions))
  async uploadIpfs(
    @UploadedFiles() files: Express.Multer.File[],
    @Body() createFileDto: CreateFileDto,
  ) {
    return await this.commonService.uploadIpfs(files, createFileDto.metadata);
  }

  @Get('get-ipfs')
  async getIpfs(@Query('hash') hash: string) {
    return await this.commonService.getFromIpfs(hash);
  }

  @Get('get-file-ipfs')
  async getFileFromIpfs(@Query('hash') hash: string, @Res() res: Response) {
    return await this.commonService.getFileFromIpfs(hash, res);
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

  @Post('search-all')
  async searchAll(@Body() input: SearchAllDto) {
    return this.commonService.searchAll(input);
  }
}
