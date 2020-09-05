/* eslint-disable import/newline-after-import,import/first */
import express from 'express';
require('express-async-errors');

import dotenv from 'dotenv-flow';
import bodyParser from 'body-parser';
import covid19Dao from './model/covid19Dao';
import { apiRoutes } from './routes/api';

dotenv.config();

covid19Dao.connectToDatabase().catch((err) => {
  console.log('Error connecting to database: ', err);
  process.exit();
});

const app = express();

app.use(bodyParser.json());

app.use(apiRoutes);

const SERVER_PORT = Number(process.env.PORT || 3000);
console.log(`Server is starting on port ${SERVER_PORT}`);
const server = app.listen(SERVER_PORT, (err) => {
  console.log(err || `Server started on port ${SERVER_PORT}`);
});

export default server;
