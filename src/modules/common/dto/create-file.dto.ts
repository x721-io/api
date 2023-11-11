export class CreateFileDto {
    readonly file: Express.Multer.File[];
    readonly metadata: any; // Replace with a more specific type as needed
  }