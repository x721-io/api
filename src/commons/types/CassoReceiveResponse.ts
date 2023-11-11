export interface Casso {
  error: number;
  data: CassoReceive[];
}

export interface CassoReceive {
  id: number;
  tid: string;
  description: string;
  amount: number;
  cusum_balance: number;
  when: Date;
  bank_sub_acc_id: string;
  subAccId: string;
  virtualAccount: string;
  virtualAccountName: string;
  corresponsiveName: string;
  corresponsiveAccount: string;
  corresponsiveBankId: string;
  corresponsiveBankName: string;
}
