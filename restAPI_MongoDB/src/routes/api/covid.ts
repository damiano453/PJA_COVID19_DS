import { Router } from 'express';
import covid19Dao from '../../model/covid19Dao';

const route = Router();
route.post('/api/covid/query', async (req, res) => {
  try {
    const { query, limit } = req.body;
    if (query === undefined) {
      res.status(400);
      res.json({
        status: 'error',
        message: 'Your request should contain query parameter'
      });
      return;
    }
    const result = await covid19Dao.makeQuery(query, limit);
    res.json(result);
  } catch (e) {
    res.status(500);
    res.json({
      status: 'error',
      message: 'An error happened while making a query'
    });
  }
});

export { route as covidRoute };

