import {
  registerDecorator,
  ValidationOptions,
  ValidationArguments,
} from 'class-validator';

export function IsHtmlWithoutScript(validationOptions?: ValidationOptions) {
  return function (object: any, propertyName: string) {
    registerDecorator({
      name: 'isHtmlWithoutScript',
      target: object.constructor,
      propertyName: propertyName,
      options: validationOptions,
      validator: {
        validate(value: any, args: ValidationArguments) {
          // Check if the value contains the script tag
          if (/<script.*?>.*?<\/script>/i.test(value)) {
            return false; // If contains script tag, return false
          }
          return true; // Otherwise, return true
        },
        defaultMessage(args: ValidationArguments) {
          return `${args.property} should be HTML without script tags`;
        },
      },
    });
  };
}
