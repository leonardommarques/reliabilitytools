import pymongo
import pandas as pd
import datetime

def analysis_collection_mongo_to_r(
query_doc=''
, collection_name=''
, db_name=''
):

client = pymongo.MongoClient()
db = client[db_name]
docs_db = db.get_collection(collection_name).find(query_doc)

print(query_doc)
print(collection_name)
print(db_name)
print(type(docs_db))

docs_list = []
for doc_db in docs_db:

    # -- dates to string -- #
    print(doc_db['analysis_period'])
    print(type(doc_db['analysis_period']))
    doc_db['analysis_period'] = list(map(lambda x: x.strftime("%Y-%m-%d"), doc_db['analysis_period']))

    print('periodo transformado')

    # -- change DataFrames to data.frames, and mutate the dates to strings.
    doc_db['ff_table'] = pd.DataFrame(doc_db['ff_table'])
    doc_db['ff_table']['month'] = doc_db['ff_table']['month'].map(lambda x: x.strftime("%Y-%m-%d"))
    doc_db['ff_table'] = doc_db['ff_table'].to_dict(orient='records')

    doc_db['surv_predictions'] = pd.DataFrame(doc_db['surv_predictions'])
    doc_db['surv_predictions']['month'] = doc_db['surv_predictions']['month'].map(lambda x: x.strftime("%Y-%m-%d"))
    doc_db['surv_predictions'] = doc_db['surv_predictions'].to_dict(orient='records')

    docs_list.append(doc_db)

return(doc_db)