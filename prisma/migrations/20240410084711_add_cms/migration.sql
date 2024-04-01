-- DropIndex
DROP INDEX "NFT_u2uId_collectionId_idx";

-- AlterTable
ALTER TABLE "Collection" ADD COLUMN     "isActive" BOOLEAN NOT NULL DEFAULT true;

-- AlterTable
ALTER TABLE "NFT" ADD COLUMN     "isActive" BOOLEAN NOT NULL DEFAULT true;

-- AlterTable
ALTER TABLE "Project" ADD COLUMN     "isDelete" BOOLEAN NOT NULL DEFAULT false,
ALTER COLUMN "isActivated" SET DEFAULT true;

-- AlterTable
ALTER TABLE "RoundInfo" ADD COLUMN     "isActive" BOOLEAN NOT NULL DEFAULT true,
ADD COLUMN     "isDelete" BOOLEAN NOT NULL DEFAULT false;

-- AlterTable
ALTER TABLE "User" ADD COLUMN     "isActive" BOOLEAN NOT NULL DEFAULT true;

-- CreateTable
CREATE TABLE "Account" (
    "id" UUID NOT NULL,
    "avatar" TEXT,
    "fullName" TEXT,
    "password" TEXT NOT NULL,
    "email" TEXT,
    "username" TEXT NOT NULL,
    "createdAt" TIMESTAMP(3) NOT NULL DEFAULT CURRENT_TIMESTAMP,
    "updatedAt" TIMESTAMP(3),
    "twitterLink" TEXT,
    "telegramLink" TEXT,
    "phone" TEXT,
    "roles" TEXT[] DEFAULT ARRAY['VIEWER']::TEXT[],
    "isActive" BOOLEAN NOT NULL DEFAULT true,
    "isDelete" BOOLEAN NOT NULL DEFAULT false,

    CONSTRAINT "Account_pkey" PRIMARY KEY ("id")
);

-- CreateTable
CREATE TABLE "Role" (
    "id" BIGSERIAL NOT NULL,
    "name" TEXT NOT NULL,
    "keyRole" TEXT NOT NULL,
    "createdAt" TIMESTAMP(3) NOT NULL DEFAULT CURRENT_TIMESTAMP,
    "updatedAt" TIMESTAMP(3),

    CONSTRAINT "Role_pkey" PRIMARY KEY ("id")
);

-- CreateTable
CREATE TABLE "LogAction" (
    "id" BIGSERIAL NOT NULL,
    "detail" TEXT NOT NULL,
    "time" TIMESTAMP(3) NOT NULL DEFAULT CURRENT_TIMESTAMP,
    "accountId" UUID NOT NULL,

    CONSTRAINT "LogAction_pkey" PRIMARY KEY ("id")
);

-- CreateTable
CREATE TABLE "Topic" (
    "id" BIGSERIAL NOT NULL,
    "nameTopic" TEXT NOT NULL,
    "createdAt" TIMESTAMP(3) NOT NULL DEFAULT CURRENT_TIMESTAMP,
    "createdBy" UUID NOT NULL,
    "isActive" BOOLEAN NOT NULL DEFAULT true,
    "isDelete" BOOLEAN NOT NULL DEFAULT false,

    CONSTRAINT "Topic_pkey" PRIMARY KEY ("id")
);

-- CreateTable
CREATE TABLE "Blog" (
    "id" UUID NOT NULL,
    "title" TEXT NOT NULL,
    "createdAt" TIMESTAMP(3) NOT NULL DEFAULT CURRENT_TIMESTAMP,
    "createdBy" UUID NOT NULL,
    "description" TEXT,
    "content" VARCHAR(35000),
    "isActive" BOOLEAN NOT NULL DEFAULT true,
    "isDelete" BOOLEAN NOT NULL DEFAULT false,

    CONSTRAINT "Blog_pkey" PRIMARY KEY ("id")
);

-- CreateTable
CREATE TABLE "BlogTopic" (
    "id" BIGSERIAL NOT NULL,
    "topicId" BIGINT NOT NULL,
    "blogId" UUID NOT NULL,

    CONSTRAINT "BlogTopic_pkey" PRIMARY KEY ("id")
);

-- CreateIndex
CREATE UNIQUE INDEX "Account_username_key" ON "Account"("username");

-- CreateIndex
CREATE INDEX "Account_id_isActive_isDelete_idx" ON "Account"("id", "isActive", "isDelete");

-- CreateIndex
CREATE INDEX "Blog_id_isActive_isDelete_idx" ON "Blog"("id", "isActive", "isDelete");

-- CreateIndex
CREATE INDEX "NFT_u2uId_collectionId_isActive_idx" ON "NFT"("u2uId", "collectionId", "isActive");

-- CreateIndex
CREATE INDEX "Project_id_isActivated_isDelete_idx" ON "Project"("id", "isActivated", "isDelete");

-- CreateIndex
CREATE INDEX "RoundInfo_id_isActive_isDelete_idx" ON "RoundInfo"("id", "isActive", "isDelete");

-- AddForeignKey
ALTER TABLE "LogAction" ADD CONSTRAINT "LogAction_accountId_fkey" FOREIGN KEY ("accountId") REFERENCES "Account"("id") ON DELETE RESTRICT ON UPDATE CASCADE;

-- AddForeignKey
ALTER TABLE "Topic" ADD CONSTRAINT "Topic_createdBy_fkey" FOREIGN KEY ("createdBy") REFERENCES "Account"("id") ON DELETE RESTRICT ON UPDATE CASCADE;

-- AddForeignKey
ALTER TABLE "Blog" ADD CONSTRAINT "Blog_createdBy_fkey" FOREIGN KEY ("createdBy") REFERENCES "Account"("id") ON DELETE RESTRICT ON UPDATE CASCADE;

-- AddForeignKey
ALTER TABLE "BlogTopic" ADD CONSTRAINT "BlogTopic_topicId_fkey" FOREIGN KEY ("topicId") REFERENCES "Topic"("id") ON DELETE RESTRICT ON UPDATE CASCADE;

-- AddForeignKey
ALTER TABLE "BlogTopic" ADD CONSTRAINT "BlogTopic_blogId_fkey" FOREIGN KEY ("blogId") REFERENCES "Blog"("id") ON DELETE RESTRICT ON UPDATE CASCADE;

INSERT INTO "Role" ("name", "keyRole", "createdAt", "updatedAt")
SELECT 'Admin Marketplace', 'ADMIN_MARKETPLACE', CURRENT_TIMESTAMP, NULL
WHERE NOT EXISTS (
    SELECT 1 FROM "Role" WHERE "keyRole" = 'ADMIN_MARKETPLACE'
);

INSERT INTO "Role" ("name", "keyRole", "createdAt", "updatedAt")
SELECT 'Admin Launchpad', 'ADMIN_LAUNCHPAD', CURRENT_TIMESTAMP, NULL
WHERE NOT EXISTS (
    SELECT 1 FROM "Role" WHERE "keyRole" = 'ADMIN_LAUNCHPAD'
);

INSERT INTO "Role" ("name", "keyRole", "createdAt", "updatedAt")
SELECT 'Admin Collection', 'ADMIN_COLLECTION', CURRENT_TIMESTAMP, NULL
WHERE NOT EXISTS (
    SELECT 1 FROM "Role" WHERE "keyRole" = 'ADMIN_COLLECTION'
);

INSERT INTO "Role" ("name", "keyRole", "createdAt", "updatedAt")
SELECT 'Admin Blog', 'ADMIN_BLOG', CURRENT_TIMESTAMP, NULL
WHERE NOT EXISTS (
    SELECT 1 FROM "Role" WHERE "keyRole" = 'ADMIN_BLOG'
);

INSERT INTO "Role" ("name", "keyRole", "createdAt", "updatedAt")
SELECT 'Admin NFT', 'ADMIN_NFT', CURRENT_TIMESTAMP, NULL
WHERE NOT EXISTS (
    SELECT 1 FROM "Role" WHERE "keyRole" = 'ADMIN_NFT'
);

INSERT INTO "Role" ("name", "keyRole", "createdAt", "updatedAt")
SELECT 'Admin User', 'ADMIN_USER', CURRENT_TIMESTAMP, NULL
WHERE NOT EXISTS (
    SELECT 1 FROM "Role" WHERE "keyRole" = 'ADMIN_USER'
);

INSERT INTO "Role" ("name", "keyRole", "createdAt", "updatedAt")
SELECT 'Creator', 'CREATOR', CURRENT_TIMESTAMP, NULL
WHERE NOT EXISTS (
    SELECT 1 FROM "Role" WHERE "keyRole" = 'CREATOR'
);

INSERT INTO "Role" ("name", "keyRole", "createdAt", "updatedAt")
SELECT 'Viewer', 'VIEWER', CURRENT_TIMESTAMP, NULL
WHERE NOT EXISTS (
    SELECT 1 FROM "Role" WHERE "keyRole" = 'VIEWER'
);



INSERT INTO "Account" ("id","fullName","avatar", "password", "email", "username", "createdAt", "updatedAt", "twitterLink", "telegramLink", "phone", "roles")
SELECT
    'f2d659f7-aabe-4320-8246-d6e46096951e','Account Administrator','avatar', '$2b$10$YBY6Pexurx.y7iuq/IzSVOCUTZUdiJM1VzoU8Be4i4tuNgpkQMXo2', 'administrator@email.com', 'administrator', CURRENT_TIMESTAMP, NULL, NULL, NULL, NULL, '{"ADMINISTRATOR", "VIEWER"}'
WHERE NOT EXISTS (
    SELECT 1 FROM "Account" WHERE "username" = 'administrator'
);
