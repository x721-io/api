import { HttpException, HttpStatus } from '@nestjs/common';

// Multer upload options
export const multerOptions = {
  // Enable file size limits
  // Check the mimetypes to allow for upload
  fileFilter: (req: any, file: any, cb: any) => {
    const allowedTypes = [
      'image/jpeg',
      'image/png',
      'image/gif',
      'image/bmp',
      'image/tiff',
      'image/tiff',
      'image/webp',
      'image/svg+xml',
      'audio/mpeg',
      'audio/mp4',
      'video/mp4',
      'video/x-matroska',
      'video/x-msvideo',
      'video/quicktime',
      'video/x-ms-wmv',
    ];

    if (allowedTypes.includes(file.mimetype)) {
      // Allow storage of file
      cb(null, true);
    } else {
      // Reject file
      cb(
        new HttpException(
          `Supported MIME Types: jpeg, png, gif, bmp, tiff, webp, svg, mpeg, mp4, matroska, x-msvideo, quicktime, x-ms-wmv`,
          HttpStatus.BAD_REQUEST,
        ),
        false,
      );
    }
  },
};
