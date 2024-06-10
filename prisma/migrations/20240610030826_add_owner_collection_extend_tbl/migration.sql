-- CreateTable
CREATE TABLE "Owner" (
    "id" TEXT NOT NULL,
    "ownerId" TEXT NOT NULL,
    "tokenId" TEXT NOT NULL,
    "quantity" INTEGER NOT NULL DEFAULT 0,
    "timestamp" INTEGER NOT NULL DEFAULT 0,

    CONSTRAINT "Owner_pkey" PRIMARY KEY ("id")
);

-- CreateTable
CREATE TABLE "SubgraphOwnerExtend" (
    "id" UUID NOT NULL,
    "isSync" BOOLEAN NOT NULL DEFAULT true,
    "type" "CONTRACT_TYPE" NOT NULL,
    "lastTimeSync" INTEGER NOT NULL DEFAULT 0,
    "subgraphUrl" TEXT NOT NULL,
    "address" TEXT NOT NULL,

    CONSTRAINT "SubgraphOwnerExtend_pkey" PRIMARY KEY ("id")
);
