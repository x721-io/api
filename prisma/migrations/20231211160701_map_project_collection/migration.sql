/*
  Warnings:

  - A unique constraint covering the columns `[projectId]` on the table `Collection` will be added. If there are existing duplicate values, this will fail.

*/
-- AlterTable
ALTER TABLE "Collection" ADD COLUMN     "projectId" UUID;

-- CreateIndex
CREATE UNIQUE INDEX "Collection_projectId_key" ON "Collection"("projectId");

-- AddForeignKey
ALTER TABLE "Collection" ADD CONSTRAINT "Collection_projectId_fkey" FOREIGN KEY ("projectId") REFERENCES "Project"("id") ON DELETE SET NULL ON UPDATE CASCADE;
