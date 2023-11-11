import { TErrorMode } from './MyError.error';
import { MyError } from './MyError.error';
import { EErrorCode } from '../../constants/enums/Error.enum';

interface Option {
  errorInfo?: { code?: EErrorCode; message?: string };
  errors?: {
    property: string;
    error: string;
    note?: string;
  }[];
  role?: TErrorMode;
}

class ParametersError extends MyError {
  name = 'ParametersError';

  message = 'Parameters of request invalid';

  code = EErrorCode.PARAMETERS_ERROR;

  role: TErrorMode = 'CLIENT_RESPONSE';

  status = '400';

  info: any;

  constructor(option?: Option) {
    super();
    if (option?.errorInfo?.code) this.code = option?.errorInfo?.code;
    if (option?.errorInfo?.message) this.message = option?.errorInfo.message;
    if (option?.role) this.role = option.role;
    this.info = option?.errors;
  }
}

export { ParametersError };
