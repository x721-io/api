import { User } from '@prisma/client';

export class UserEntity implements User{
    id: string;
    email: string;
    username: string;
    publicKey: string;
    signature: string;
    signedMessage: string;
    signDate: Date;

    constructor(partial: Partial<User>) {
        Object.assign(this, partial);
    }
    acceptedTerms: boolean;
    avatar: string;
    createdAt: Date;
    updatedAt: Date;
    signer: string;
}