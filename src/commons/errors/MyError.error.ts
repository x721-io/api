type TErrorMode = 'CLIENT_RESPONSE' | 'SERVER_ERROR_LOGS' | 'PARAMETERS_ERROR';

class MyError extends Error {
  message: string;

  role: TErrorMode;

  status?: string;

  code?: string;
}

export { MyError, TErrorMode };
