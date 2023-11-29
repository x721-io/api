import { PipeTransform, Injectable } from '@nestjs/common';

@Injectable()
export class LowercasePipe implements PipeTransform {
  transform(value: any): any {
    if (typeof value === 'string') {
      return value.toLowerCase();
    }
    return value;
  }
}
