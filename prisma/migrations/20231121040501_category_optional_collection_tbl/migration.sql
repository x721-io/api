-- DropForeignKey
ALTER TABLE "Collection" DROP CONSTRAINT "Collection_categoryId_fkey";

-- AlterTable
ALTER TABLE "Collection" ALTER COLUMN "categoryId" DROP NOT NULL;

-- AddForeignKey
ALTER TABLE "Collection" ADD CONSTRAINT "Collection_categoryId_fkey" FOREIGN KEY ("categoryId") REFERENCES "Category"("id") ON DELETE SET NULL ON UPDATE CASCADE;
