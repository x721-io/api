interface ServiceResponseOption<T> {
  data?: T;
  paging?: {
    limit: number;
    total: number;
    page: number;
  };
  defaultData?: any;
  count?: number;
}
export class MyServiceResponse<T> {
  data?: T;

  paging?: {
    limit: number;
    total: number;
    page: number;
  };

  [key: string]: any;

  count?: number;

  constructor(option?: ServiceResponseOption<T>) {
    this.data = option?.data || option?.defaultData;
    this.paging = option?.paging;
    this.count = option?.count;
  }
}
