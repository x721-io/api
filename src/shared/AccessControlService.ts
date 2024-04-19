import { Injectable } from '@nestjs/common';
import { Role } from '../constants/enums/role.enum';

interface IsAuthorizedParams {
  currentRole: Role;
  requiredRole: Role;
}

@Injectable()
export class AccessControlService {
  private hierarchies: Array<Map<string, number>> = [];
  constructor() {
    this.buildRoles([
      Role.VIEWER,
      Role.CREATOR,
      Role.ADMIN_BLOG,
      Role.ADMINISTRATOR,
    ]);
    this.buildRoles([
      Role.VIEWER,
      Role.ADMIN_COLLECTION,
      Role.ADMIN_MARKETPLACE,
      Role.ADMINISTRATOR,
    ]);
    this.buildRoles([
      Role.VIEWER,
      Role.ADMIN_NFT,
      Role.ADMIN_MARKETPLACE,
      Role.ADMINISTRATOR,
    ]);
    this.buildRoles([
      Role.VIEWER,
      Role.ADMIN_USER,
      Role.ADMIN_MARKETPLACE,
      Role.ADMINISTRATOR,
    ]);
    this.buildRoles([Role.VIEWER, Role.ADMIN_LAUNCHPAD, Role.ADMINISTRATOR]);
  }

  private buildRoles(roles: Role[]) {
    const hierarchy: Map<string, number> = new Map();
    roles.forEach((role, index) => {
      hierarchy.set(role, index + 1);
    });
    this.hierarchies.push(hierarchy);
  }

  public isAuthorized({ currentRole, requiredRole }: IsAuthorizedParams) {
    const { rolePriority, roleText } = this.getHighestPermission(currentRole);
    let mRequiredpriority = 0;
    for (const hierarchy of this.hierarchies) {
      const requiredPriority = hierarchy.get(requiredRole);
      const currentPriority = hierarchy.get(roleText);
      if (requiredPriority && currentPriority) {
        mRequiredpriority = Math.max(requiredPriority, mRequiredpriority);
        if (
          rolePriority &&
          mRequiredpriority &&
          (rolePriority > mRequiredpriority ||
            (rolePriority == mRequiredpriority && roleText == requiredRole))
        ) {
          return true;
        }
        break;
      }
    }
    return false;
  }
  public getHighestPermission(currentRole: Role) {
    let rolePriority = 0;
    let roleText = '';
    for (const hierarchy of this.hierarchies) {
      for (const role of currentRole) {
        if (hierarchy.get(role)) {
          if (rolePriority < hierarchy.get(role)) {
            rolePriority = hierarchy.get(role);
            roleText = role;
          }
        }
      }
    }
    return { roleText, rolePriority };
  }
}
