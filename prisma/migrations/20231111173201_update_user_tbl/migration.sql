-- AlterTable
ALTER TABLE "User" ALTER COLUMN "email" DROP NOT NULL,
ALTER COLUMN "username" DROP NOT NULL,
ALTER COLUMN "signature" DROP NOT NULL,
ALTER COLUMN "signedMessage" DROP NOT NULL,
ALTER COLUMN "publicKey" DROP NOT NULL,
ALTER COLUMN "signDate" DROP NOT NULL;
