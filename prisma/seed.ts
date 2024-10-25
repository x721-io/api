import { PrismaClient } from '@prisma/client'
const prisma = new PrismaClient()
async function main() {
    const migrations = await prisma.$queryRaw `DELETE FROM "_prisma_migrations" where "migration_name" = '20241025100429_default_data'`;
    console.log("🚀 ~ main ~ migrations:", migrations)
    console.log("Done");
}
main()
  .then(async () => {
    await prisma.$disconnect()
  })
  .catch(async (e) => {
    console.error(e)
    await prisma.$disconnect()
    process.exit(1)
  })