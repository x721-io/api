import { TErrorMode } from './MyError.error';
import { EErrorCode } from '../../constants/enums/Error.enum';

interface Option {
  errorInfo?: { code?: EErrorCode; message?: string };
}

export default class OtherError extends Error {
  name = 'OtherError';

  message: string;

  code = EErrorCode.OTHER_ERROR;

  status = 500;

  info?: any;

  role: TErrorMode = 'CLIENT_RESPONSE';

  constructor(option: Option) {
    super();
    if (option?.errorInfo?.code) this.code = option?.errorInfo?.code;
    if (option?.errorInfo?.message) this.message = option?.errorInfo.message;
  }
}
