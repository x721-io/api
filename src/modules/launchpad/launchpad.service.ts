import { Injectable, NotFoundException } from '@nestjs/common';
import { CreateLaunchpadDto } from './dto/create-launchpad.dto';
import { UpdateLaunchpadDto } from './dto/update-launchpad.dto';
import { PrismaService } from 'src/prisma/prisma.service';
import { ProjectEntity } from './entities/project.entity';
import { RoundEntity } from './entities/round.entity';
import { FindAllProjectDto } from './dto/find-all-project.dto';

@Injectable()
export class LaunchpadService {
  constructor(private readonly prisma: PrismaService) {}
  // create(createLaunchpadDto: CreateLaunchpadDto) {
  //   return 'This action adds a new launchpad';
  // }

  async findAll(query: FindAllProjectDto): Promise<any> {
    const projects = await this.prisma.project.findMany({
      where: {
        isActivated: true,
      },
      include: {
        collection: true,
        rounds: {
          where: {
            start: { gte: new Date(query.start) },
            end: { lte: new Date(query.end) },
          },
          include: {
            round: true,
          },
        },
      },
    });
    // if (!projects)
    const returnProject = projects.map((item) => {
      return {
        ...item,
        rounds: item.rounds.map((round) => ({
          ...round,
          ...round.round,
        })),
      };
    });
    // console.log(returnProject[0].rounds)
    return returnProject;
  }

  async findOne(id: string): Promise<ProjectEntity> {
    const project = await this.prisma.project.findUnique({
      where: {
        id,
      },
      include: {
        collection: true,
        rounds: {
          include: {
            round: true,
          },
        },
      },
    });
    if (!project) throw new NotFoundException('Project not found');
    return {
      ...project,
      rounds: project.rounds.map((round) => ({
        ...round,
        ...round.round,
      })),
    };
  }
}
