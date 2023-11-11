import * as PromiseBluebird from 'bluebird';
import Redis, { Cluster, RedisOptions } from 'ioredis';
import { messageConstant } from '../constants';
import { logger } from '../commons';

class RedisDB {
  clientSync: Redis.Redis | Cluster;
  subscriberSync: Redis.Redis | Cluster;
  client: any;
  subscriber: any;

  public listRedisNode: any[] = [{}];
  public redisConfig: RedisOptions;

  public redisConfigSetter(host, port, password) {
    this.redisConfig = {
      port,
      host,
      password,
    };
  }

  public async connect(): Promise<void> {
    await this.getSetConnect();
  }

  public async getClient(): Promise<Redis.Redis | Cluster> {
    if (!this.clientSync) {
      await this.getSetConnect();
    }
    return this.clientSync;
  }

  public async getKeyTTL(key: string): Promise<number> {
    const client = await this.getClient();
    return new Promise((resolve, reject) => {
      client.ttl(key, (err, ttl) => {
        if (err) {
          reject(err);
        } else {
          resolve(ttl);
        }
      });
    });
  }

  public async getSubscriberClient(): Promise<Redis.Redis | Cluster> {
    if (!this.subscriberSync) {
      await this.connectSubscriber();
    }
    return this.subscriberSync;
  }

  public async getSetConnect(): Promise<void> {
    await new Promise(async (resolve) => {
      if ((process.env.REDIS_CLUSTER as string) === '1') {
        this.clientSync = new Redis.Cluster(this.listRedisNode);
        this.subscriberSync = new Redis.Cluster(this.listRedisNode);
        logger.info(
          `${messageConstant.SERVER_MESSAGE.REDIS_CONNECTING}: ${process.env.REDIS_CLUSTER_NODE}`,
        );
      } else {
        this.redisConfigSetter(
          process.env.REDISDB_HOST as string,
          process.env.REDISDB_PORT,
          process.env.REDIS_PASSWORD,
        );
        this.clientSync = new Redis(this.redisConfig);
        this.subscriberSync = new Redis(this.redisConfig);
        logger.info(
          `${messageConstant.SERVER_MESSAGE.REDIS_CONNECTING}: ${this.redisConfig.host}:${this.redisConfig.port}`,
        );
      }
      let reconnect = 0;
      this.clientSync.on('error', function (err: any) {
        reconnect = reconnect + 1;
        logger.info(String(reconnect));
        logger.error(err.stack);
      });
      this.clientSync.on('connect', async () => {
        this.client = PromiseBluebird.promisifyAll(this.clientSync as object);
        logger.info(messageConstant.SERVER_MESSAGE.REDIS_CONNECTED);
        resolve(null);
      });
    });
  }

  public async connectSubscriber(): Promise<void> {
    await new Promise(async (resolve) => {
      this.subscriberSync.on('error', function (err: any) {
        logger.error(err.stack);
      });
      this.subscriberSync.on('connect', async () => {
        this.subscriber = PromiseBluebird.promisifyAll(
          this.subscriberSync as object,
        );
        logger.info(messageConstant.SERVER_MESSAGE.REDIS_SUBSCRIBER_CONNECTED);
        resolve(null);
      });
    });
  }

  public async subscribe(
    channel: string,
    callback: (channel: string, message: any) => void,
  ): Promise<void> {
    if (!this.subscriber) {
      await this.connectSubscriber();
    }
    await this.subscriber.subscribe(channel);
    this.subscriber.on('message', callback);
  }

  public async unsubscribe(channel: string): Promise<void> {
    if (!this.subscriber) {
      await this.connectSubscriber();
    }
    await this.subscriber.unsubscribe(channel);
  }

  public async publish(channel: string, message: any): Promise<number> {
    const client = await this.getClient();
    return client.publish(channel, JSON.stringify(message));
  }
}

export default new RedisDB();
