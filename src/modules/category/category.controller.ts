
import { Controller, Get, Post, Body, Patch, Param, Delete, Query , UseGuards, Put} from '@nestjs/common';
import { AuthenticationGuard } from '../auth/guards/auth.guard';
import { GetCurrentUser } from 'src/decorators/get-current-user.decorator';
import { User } from '@prisma/client';
import { CreateCategoryDto } from './dto/create-category.dto';
import { UpdateCategoryDto } from './dto/update-category.dto';
import { GetCategoryDto } from './dto/get-categort.dto';
import { CategoryService } from './category.service';


@Controller('category')
export class CategoryController {
  constructor(private readonly categoryService : CategoryService){}

  @Post()
  @UseGuards(AuthenticationGuard)
  create(@Body() createCategoryDto : CreateCategoryDto ,  @GetCurrentUser() user: User ) {
    return this.categoryService.create(createCategoryDto , user);
  }

  @Get()
  findAll(){
    return this.categoryService.findAll();
  }

  @Get(':id')
  findOne(@Param('id') id : Number){
    return this.categoryService.findOne(+id);
  }

  @Put(':id')
  @UseGuards(AuthenticationGuard)
  update(@Param('id') id : Number , @Body() updateCategoryDto : UpdateCategoryDto , @GetCurrentUser() user: User  ){
    return this.categoryService.update(+id , updateCategoryDto , user);
  }
}
