import { Storage } from '@google-cloud/storage';
import { createReadStream } from 'fs';
import logger from './Logger.common';
import { FileUpload } from './types/Fileupload.common';
import { v4 as uuidv4 } from 'uuid';
import * as sharp from 'sharp';

// Commons
// Models

class GCPCommonCommon {
  storage: Storage;

  constructor() {
    this.init();
  }

  private init() {
    this.storage = new Storage({
      projectId: 'congtroi',
      keyFilename: 'vucar-key.json',
      //   secretAccessKey: process.env.AWS_SECRET_KEY,
    });
  }

  // public async upload(files: FileUpload, setFilename: string) {
  //   try {
  //     if (files) {
  //       const { createReadStream, filename } = await files;
  //       const stream = createReadStream();
  //       const chunks = [];
  //       const timestamp = new Date().toISOString().replace(/\.\d+Z$/, '');
  //       const url = await new Promise<string>(async (resolve, reject) => {
  //         let buffer: Buffer;
  //         const bucket = await this.storage.bucket(process.env.BUCKET_NAME);
  //         const file = bucket.file(
  //           `${setFilename}_${timestamp}_${uuidv4()}_${filename}`,
  //         );
  //         stream.on('data', (data) => {
  //           chunks.push(data);
  //         });
  //         stream.on('end', async () => {
  //           buffer = Buffer.concat(chunks);
  //           // Use sharp to optimize the image
  //           const optimizedBuffer = await sharp(buffer)
  //             .webp({ quality: 95 }) // Example: Convert to JPEG with 80% quality. You can adjust this.
  //             .toBuffer();
  //           const publicUrl = await new Promise(async (resolve, reject) => {
  //             file
  //               .save(optimizedBuffer)
  //               .then(() => {
  //                 file
  //                   .makePublic()
  //                   .then(() => {
  //                     resolve(`${process.env.CDN_URL}/${file.name}`);
  //                   })
  //                   .catch((err) => {
  //                     reject(err);
  //                   });
  //               })
  //               .catch((err) => {
  //                 reject(err);
  //               });
  //           });
  //           resolve(publicUrl as string);
  //         });
  //       });
  //       return { Location: url as string };
  //     } else throw new Error('File is not included');
  //   } catch (err) {
  //     logger.error(err.message);
  //     throw new Error('There is something wrong while uploading asset');
  //   }
  // }

  public async upload(files: FileUpload, setFilename: string) {
    try {
      if (files) {
        const { createReadStream, filename } = await files;
        const stream = createReadStream();
        const chunks = [];
        const timestamp = new Date().toISOString().replace(/\.\d+Z$/, '');
        const fileExtension = filename.split('.').pop();
        const uniqueName = `${setFilename}_${timestamp}_${uuidv4()}`;

        const url = await new Promise<string>(async (resolve, reject) => {
          let buffer: Buffer;
          const bucket = await this.storage.bucket(process.env.BUCKET_NAME);

          stream.on('data', (data) => {
            chunks.push(data);
          });

          stream.on('end', async () => {
            buffer = Buffer.concat(chunks);

            let outputFileExtension = fileExtension;
            let outputBuffer = buffer;

            // Check if the file is an image (by file extension)
            if (['jpeg', 'jpg', 'png', 'tiff'].includes(fileExtension)) {
              try {
                // It's an image, try to convert it to webp
                outputBuffer = await sharp(buffer)
                  .webp({ quality: 95 })
                  .toBuffer();
                outputFileExtension = 'webp';
              } catch (err) {
                // Log an error if sharp cannot process this image
                logger.error(`Error processing image: ${err.message}`);
              }
            }

            const file = bucket.file(`${uniqueName}.${outputFileExtension}`);

            const publicUrl = await new Promise(async (resolve, reject) => {
              file
                .save(outputBuffer)
                .then(() => {
                  file
                    .makePublic()
                    .then(() => {
                      resolve(`${process.env.CDN_URL}/${file.name}`);
                    })
                    .catch((err) => {
                      reject(err);
                    });
                })
                .catch((err) => {
                  reject(err);
                });
            });
            resolve(publicUrl as string);
          });
        });
        return { Location: url as string };
      } else throw new Error('File is not included');
    } catch (err) {
      logger.error(err.message);
      throw new Error('There is something wrong while uploading asset');
    }
  }

  public async deleteAsset(fileName) {
    try {
      await this.storage.bucket('vucar-user-assets').file(fileName).delete();
      logger.info(
        `Successfully deleted ${fileName} from ${'vucar-user-assets'}`,
      );
    } catch (error) {
      logger.info(
        `Failed to delete ${fileName} from ${'vucar-user-assets'}`,
        error,
      );
      throw error;
    }
  }
}

export default new GCPCommonCommon();
