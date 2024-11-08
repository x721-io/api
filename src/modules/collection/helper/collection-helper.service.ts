import { ApiCallerService } from 'src/modules/api-caller/api-caller.service';
import * as moment from 'moment';

class CollectionHepler {
  apiService: ApiCallerService;

  getCurrentDay() {
    const date = new Date();
    const momentDate = moment(date);
    return {
      start: momentDate.utc().startOf('day').toDate(),
      end: momentDate.utc().endOf('day').toDate(),
    };
  }

  getPastDay(number: number) {
    const date = new Date();
    const momentDate = moment(date).subtract(number, 'days');
    return {
      start: momentDate.utc().startOf('day').toDate(),
      end: momentDate.utc().endOf('day').toDate(),
    };
  }

  removeDuplicates(array, key) {
    const seen = new Set();
    return array.filter((item) => {
      const value = key.split('.').reduce((o, k) => (o || {})[k], item);
      if (seen.has(value)) {
        return false;
      } else {
        seen.add(value);
        return true;
      }
    });
  }
}

export default new CollectionHepler();
