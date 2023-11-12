import { User } from '@prisma/client';

export class UserEntity implements User{
    id: number;
    email: string;
    username: string;
    publicKey: string;
    signature: string;
    signedMessage: string;
    signDate: Date;

    constructor(partial: Partial<User>) {
        Object.assign(this, partial);
    }
    createdAt: Date;
    updatedAt: Date;
    signer: string;
}