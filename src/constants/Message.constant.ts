import { EErrorCode } from './enums/Error.enum';

const ERROR_SERVER = 'Something went wrong';
const QUEUE_CONNECTING = 'U2U queue connecting';
const QUEUE_CONNECTED = 'U2U queue connected';
const QUEUE_CONNECT_ERROR = 'U2U queue cannot connect';

const SERVER_MESSAGE = {
  REDIS_CONNECTING: 'Redis database connecting',
  REDIS_CONNECTED: 'Redis database connected',
  REDIS_SUBSCRIBER_CONNECTED: 'Redis subscriber connected',
};

const PARAMETER_ERROR = {
  EMAIL_EXISTS: {
    message: 'Email is exists!',
  },
  INVALID_PRICE: {
    message: 'Price is invalid!',
  },
  INVALID_BUNDLE: {
    message: 'Bundle is invalid!',
  },
  INVALID_BUNDLE_TYPE: {
    message: 'Bundle type is invalid!',
  },
};
const OTHER_ERROR = {
  CREATE_USER_FAILED: {
    code: EErrorCode.REGISTER_ERROR,
    message: 'Create users failed!',
  },
  LOGIN_OAUTH_FAIL: {
    code: EErrorCode.REGISTER_ERROR,
    message: 'Login failed!',
  },
  CAR_IS_NOT_AVAILABLE: {
    message: 'Car is not available',
  },
};

const SERVER_ERROR = {
  ERROR_DEFAULT: 'Something went wrong',
};

export default {
  SERVER_MESSAGE,
  SERVER_ERROR,
  PARAMETER_ERROR,
  OTHER_ERROR,
  ERROR_SERVER,
  QUEUE_CONNECTED,
  QUEUE_CONNECTING,
  QUEUE_CONNECT_ERROR,
};
