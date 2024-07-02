-- AlterTable
ALTER TABLE "Collection" ADD COLUMN     "metricDetail" JSONB DEFAULT '{"Verified":0,"Volume":{"key":"volume_lv0","value":"0","point":0,"total":0},"TotalUniqueOwner":{"key":"owner_lv0","value":"0","point":0,"total":0},"TotalItems":{"key":"item_lv0","value":0,"point":0,"total":0},"Followers":{"key":"follower_lv0","value":0,"point":0,"total":0}}',
ADD COLUMN     "metricPoint" BIGINT DEFAULT 0;

-- AlterTable
ALTER TABLE "MarketplaceStatus" ADD COLUMN     "metricPoint" BIGINT DEFAULT 0;

-- AlterTable
ALTER TABLE "NFT" ADD COLUMN     "metricDetail" JSONB DEFAULT '{"VolumeIndividual":0,"UserMetric":0}',
ADD COLUMN     "metricPoint" BIGINT DEFAULT 0;

-- AlterTable
ALTER TABLE "User" ADD COLUMN     "metricDetail" JSONB DEFAULT '{"Verified":0,"Followers":{"key":"follower_lv0","value":0,"point":0,"total":0},"CollectionMetric":0,"VolumeIndividual":0}',
ADD COLUMN     "metricPoint" BIGINT DEFAULT 0;
