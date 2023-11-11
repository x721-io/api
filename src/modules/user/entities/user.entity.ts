import { Prisma } from "@prisma/client";

export class User implements Prisma.UserUncheckedCreateInput{
    id: number;
    email: string;
    username: string;
    publicKey: string;
    signature: string;
    signedMessage: string;
    signDate: Date;
    createdAt?: Date | string;
    updatedAt?: Date | string;

    constructor(partial: Partial<User>) {
        Object.assign(this, partial);
    }
    signer: string;
}