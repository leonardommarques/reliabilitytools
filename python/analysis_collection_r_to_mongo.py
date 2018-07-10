# import sqlite3
# from tqdm import tqdm
# import re
# import time
# import random
# import pprint
# import numpy as np

import pymongo
import pandas as pd
import datetime


def analysis_collection_r_to_mongo(
        analysis_collection = ''
        , collection_name = ''
        , db_name = ''
):
    
    # -- mongo -- #
    client = pymongo.MongoClient()
    db = client[db_name]

    print('mongo aberto')
    print(collection_name)
    print(db_name)

    # -- document to insert
    analysis_collection = r[analysis_collection]

    print('doc lido')
    print(type(analysis_collection))
    print(type(analysis_collection['analysis_period'][0]))

    # -- convert strings to dates
    analysis_collection['analysis_period'] = list(map(lambda x: datetime.datetime.strptime(x, '%Y-%m-%d'), analysis_collection['analysis_period']))

    print('periodo convertido')

    # -- change data.frames to DataFrames, mutate the dates and then convert to dict.
    analysis_collection['ff_table'] = pd.DataFrame(analysis_collection['ff_table'])
    analysis_collection['ff_table']['month'] = analysis_collection['ff_table']['month'].map(lambda x: datetime.datetime.strptime(str(x), '%Y-%m-%d'))
    analysis_collection['ff_table'] = analysis_collection['ff_table'].to_dict(orient='records')

    analysis_collection['surv_predictions'] = pd.DataFrame(analysis_collection['surv_predictions'])
    analysis_collection['surv_predictions']['month'] = analysis_collection['surv_predictions']['month'].map(lambda x: datetime.datetime.strptime(str(x), '%Y-%m-%d'))
    analysis_collection['surv_predictions'] = analysis_collection['surv_predictions'].to_dict(orient='records')

    # -- paretos to dictionaries -- #
    for pareto_list in analysis_collection['pareto'].keys():
        for pareto in analysis_collection['pareto'][pareto_list].keys():
            analysis_collection['pareto'][pareto_list][pareto] = analysis_collection['pareto'][pareto_list][pareto].to_dict(orient='records')

    # -- Save in mongo -- #
    db.get_collection(collection_name).insert(analysis_collection)

    client.close()

    return 'ok'

