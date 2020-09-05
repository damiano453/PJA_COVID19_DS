import { Db, MongoClient } from 'mongodb';

const COLLECTION_NAME = 'covid19';

let db: Db;

async function connectToDatabase() {
  const databaseURI = process.env.MONGODB_URI;
  if (databaseURI === undefined) {
    throw Error('MONGODB_URI environment variable is not provided');
  }

  const client = new MongoClient(databaseURI, { useNewUrlParser: true });
  await client.connect();
  db = client.db();
}

async function makeQuery(queryObject: object, limit: number = 10): Promise<object> {
  return db.collection(COLLECTION_NAME)
    .find(queryObject)
    .limit(limit)
    .toArray();
}

export default { connectToDatabase, makeQuery };
