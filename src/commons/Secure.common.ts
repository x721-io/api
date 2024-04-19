import RedisCommon from '../database/Redis.db';
import logger from './Logger.common';
import { UserEntity } from 'src/modules/user/entities/user.entity';
import { Account } from '@prisma/client';

class SecureUtil {
  public async storeSession(
    userInfo: UserEntity,
    refreshToken: string,
  ): Promise<number> {
    const expire = Math.floor(Date.now() + 604800000);
    try {
      await RedisCommon.client.setAsync(
        `session:${refreshToken}`,
        userInfo.id,
        'EX',
        604800,
      );
    } catch (err) {
      throw new Error(err);
    }
    return expire;
  }

  public async storeSessionAccount(
    userInfo: Account,
    refreshToken: string,
  ): Promise<number> {
    const expire = Math.floor(Date.now() + 604800000);
    try {
      await RedisCommon.client.setAsync(
        `session:${refreshToken}`,
        userInfo.id,
        'EX',
        604800,
      );
    } catch (err) {
      throw new Error(err);
    }
    return expire;
  }

  public async storeObjectSession<K, V>(
    key: K,
    value: V,
    expire: number,
  ): Promise<number> {
    // const expire = Math.floor(Date.now() + 604800000);
    try {
      await RedisCommon.client.setAsync(
        `session:${key}`,
        JSON.stringify(value),
        'EX',
        expire,
      );
    } catch (err) {
      throw new Error(err);
    }
    return Math.floor(Date.now() + expire * 1000);
  }

  public async getSessionInfo(token: string): Promise<string> {
    // let authInfo: AuthInfoPayload | undefined;
    try {
      const refreshToken = await RedisCommon.client.getAsync(
        `session:${token}`,
      );
      return refreshToken;
    } catch (err) {
      logger.info(err);
      return null;
    }
  }

  public async getSessionExpiration(token: string): Promise<number> {
    // let authInfo: AuthInfoPayload | undefined;
    try {
      const refreshToken = await RedisCommon.getKeyTTL(`session:${token}`);
      return refreshToken;
    } catch (err) {
      logger.info(err);
      return null;
    }
  }

  public async deleteSessionInfo(token: string): Promise<boolean> {
    try {
      await RedisCommon.client.delAsync(`session:${token}`);
      return true;
    } catch (err) {
      logger.info(err);
      return false;
    }
  }
}
export default new SecureUtil();
