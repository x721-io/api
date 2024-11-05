import { HttpException, HttpStatus } from '@nestjs/common';

// Multer upload options
export const multerOptions = {
  // Enable file size limits
  // Check the mimetypes to allow for upload
  fileFilter: (req: any, file: any, cb: any) => {
    const allowedTypes = [
      'image/png',
      'image/gif',
      'image/bmp',
      'image/tiff',
      'image/webp',
      'audio/mp3',
      'audio/mpeg',
      'video/mp4',
      'video/webm',
    ];
    if (allowedTypes.includes(file.mimetype)) {
      // Allow storage of file
      cb(null, true);
    } else {
      // Reject file
      cb(
        new HttpException(
          `Supported MIME Types: png, gif, bmp, tiff, webp, svg, mpeg, mp4, mp3 ,webm`,
          HttpStatus.BAD_REQUEST,
        ),
        false,
      );
    }
  },
};
