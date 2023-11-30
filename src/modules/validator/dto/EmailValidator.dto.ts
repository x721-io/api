import { IsEmail } from 'class-validator';
export class EmailValidator {
  @IsEmail({}, { message: 'Invalid email format' })
  email: string;
}
