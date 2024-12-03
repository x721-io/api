-- CreateTable
CREATE TABLE "Platform" (
    "id" UUID NOT NULL,
    "platform" TEXT NOT NULL,
    "name" TEXT NOT NULL,
    "avatar" TEXT,
    "banner" TEXT,
    "description" TEXT,
    "creator" TEXT NOT NULL,
    "createdAt" TIMESTAMP(3) NOT NULL DEFAULT CURRENT_TIMESTAMP,
    "updatedAt" TIMESTAMP(3),

    CONSTRAINT "Platform_pkey" PRIMARY KEY ("id")
);

-- CreateTable
CREATE TABLE "OverviewTemplate" (
    "id" UUID NOT NULL,
    "platformId" UUID NOT NULL,
    "name" TEXT NOT NULL,
    "avatar" TEXT,
    "banner" TEXT,
    "description" TEXT,
    "sections" JSONB,
    "isActive" BOOLEAN NOT NULL,
    "creator" TEXT NOT NULL,
    "createdAt" TIMESTAMP(3) NOT NULL DEFAULT CURRENT_TIMESTAMP,
    "updatedAt" TIMESTAMP(3),

    CONSTRAINT "OverviewTemplate_pkey" PRIMARY KEY ("id")
);

-- AddForeignKey
ALTER TABLE "OverviewTemplate" ADD CONSTRAINT "OverviewTemplate_platformId_fkey" FOREIGN KEY ("platformId") REFERENCES "Platform"("id") ON DELETE RESTRICT ON UPDATE CASCADE;
