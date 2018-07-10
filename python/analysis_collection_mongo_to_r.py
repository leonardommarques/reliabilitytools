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
    doc_db = db.get_collection(collection_name).find_one(query_doc)

    print(query_doc)
    print(collection_name)
    print(db_name)
    print(type(doc_db))

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

    return(doc_db)

