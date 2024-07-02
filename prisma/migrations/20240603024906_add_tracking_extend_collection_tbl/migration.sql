-- AlterTable
ALTER TABLE "Collection" ADD COLUMN     "flagExtend" BOOLEAN DEFAULT false,
ADD COLUMN     "isSync" BOOLEAN DEFAULT true,
ADD COLUMN     "lastTimeSync" INTEGER DEFAULT 0,
ADD COLUMN     "subgraphUrl" TEXT;
