import {
  Injectable,
  HttpException,
  HttpStatus,
  // NotFoundException,
} from '@nestjs/common';
import { ValidatorDto } from './dto/validator.dto';
import { PrismaService } from 'src/prisma/prisma.service';
import { TypeValidator } from '../../constants/enums/Validator.enum';
import { validate as isValidUUID } from 'uuid';

@Injectable()
export class ValidatorService {
  private readonly prisma: PrismaService;

  private isEmail(input: string) {
    // Regular expression for a basic email validation
    const emailRegex = /^[^\s@]+@[^\s@]+\.[^\s@]+$/;

    // Test the input against the regular expression
    return emailRegex.test(input);
  }

  constructor(prisma: PrismaService) {
    this.prisma = prisma;
  }
  async validateUniqueField(validator: ValidatorDto) {
    try {
      const { key, value, collectionId } = validator;

      switch (key) {
        case TypeValidator.username:
        case TypeValidator.email:
        case TypeValidator.shortLink:
          const hasValue = await this.checkUserExistence(key, value);
          return !hasValue;

        case TypeValidator.CollectionName:
          const hasCLName = await this.checkCollectionExistence('name', value);
          return !hasCLName;

        case TypeValidator.shortUrl:
          const hasURL = await this.checkCollectionExistence('shortUrl', value);
          return !hasURL;
        case TypeValidator.NftName:
          if (!collectionId) {
            throw new Error('Collection ID should not be empty');
          }
          if (!isValidUUID(collectionId)) {
            throw new Error('Invalid Collection ID. Please try again !');
          }
          const hasNFTName = await this.checkNFTExistence(
            'name',
            'collectionId',
            value,
            collectionId,
          );
          return !hasNFTName;
      }
    } catch (err) {
      throw new HttpException(`${err.message}`, HttpStatus.BAD_REQUEST);
    }
  }

  async checkUserExistence(field: string, value: string) {
    const condition = { [field]: value };
    const existingValue = await this.prisma.user.findFirst({
      where: condition,
    });
    return !!existingValue;
  }

  async checkCollectionExistence(field: string, value: string) {
    const condition = { [field]: value };
    const existingValue = await this.prisma.collection.findFirst({
      where: condition,
    });
    return !!existingValue;
  }

  public async checkNFTExistence(
    field1: string,
    field2: string,
    value1: string,
    value2: string,
  ) {
    const condition = {
      [field1]: value1,
      [field2]: value2,
    };

    const existingValue = await this.prisma.nFT.findFirst({
      where: { AND: [condition] },
    });
    return !!existingValue;
  }
}
