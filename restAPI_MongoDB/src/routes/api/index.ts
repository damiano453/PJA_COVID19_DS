import { Router } from 'express';
import { covidRoute } from './covid';

const apiRoutes = Router();
apiRoutes.use(covidRoute);
export {
  apiRoutes,
};
