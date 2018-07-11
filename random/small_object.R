# ---------------- #
# MR example
# ---------------- #

library(flexsurv) 
library(tidyverse)
library(plyr)
library(data.table)
library(lubridate)
library(stringr)
library(scales)
library(grid)
library(sqldf)
require(reticulate)

# library(reliabilitytools)

w_ = list.files("/Users/leo/Documents/Estudos/R/my_packages/reliabilitytools/R/"
                , full.names = TRUE) %>%
  lapply(source)

# ---------------- #

`%+%` = function(a,b) paste0(a,b)

# -- Eviroment preparation-- #
Sys.setlocale("LC_TIME", "C")
# -- Database -- +
DATA_BASE = "/Users/leo/Documents/Estudos/R/my_packages/data/sample_data_5.db"
# reports_folder = 'C:/Temp/reliability_report/'


aux_query = list(
  select='select
"index"
, CPART
, CtryOp
, CHASNO
, VEHMIL
, FPAYDT_yearmon
, DWASSD_yearmon\n'

  # , MARTYPE
  # , CPART
  # , RDEAL
  # , FUNGRP
  # , VEHAGE
  # , Total_Cost
  # , Part_Name
  # , Customer
  # , Vendor_Dealer,RG_no
  # , CtryOp
  # , GboxType
  # , RAType
  # , EngType
  # , DesArea
  # , EngLeg
  # , MARTYPE_srt
  # , EngType_srt
  # , EngType_srt2
  # , EngType_pwr
  # , continent
  # , GboxType_srt
  # , axle_aux
  # , axle
  # , articulation
  # , DEBCOD
  
  , where='\nfrom CLAIMS\nwhere 1>0'
)


campos = sqldf('select * from CLAIMS limit 10', dbname = DATA_BASE)
campos %>% names %>% sort()

param = list(
  
  alias = 'repor to convert'
  , description = 'this is a report i am using to convert form mongo to R'
  
  , rm_size = 24
  , analysis_period = c(as.Date('2017-02-01'), as.Date('2018-04-01'))
  , filter_columns = list(MARTYPE_srt = "FH"
                          # , EngType_pwr = c(500,520,540, 550, 580)
                          , continent = 'LA')
  , classification_columns = list( FG255 = list(FG3 = c(212, 213,215)
                                                , DEBCOD = c(5, 13, 15, 10, 18)
                                                )
                                   )
)


param$data = seq.Date(param$analysis_period[1]
                      , param$analysis_period[2]
                      , by = 'months'
) %>% 
  rev()

names(param$data) = param$data


ti = Sys.time()

# -- Get Data -- #
add_columns = param$classification_columns %>% 
  map(names) %>% c() %>% unlist %>% unique() %>% paste(',',.,collapse='\n')
aux_select_statement = paste0(aux_query[[1]]
                              , add_columns
                              , aux_query[[2]]
                              , collapse='\n', sep = ''
)

param$data = param$data %>% 
  llply(function(cur_date)
    rm_data_separate_SQlite(
      data_base = DATA_BASE
      , current_month = cur_date
      , RM_size = param$rm_size
      , select_statement = aux_select_statement
      , population_filter = param$filter_columns %>% list_to_SQLfilter()
      , assemb_col = "DWASSD_yearmon"
      , repair_col = "REPDAT_yearmon"
    ), .progress = 'text'
  )

param$data_raw = param$data

# -- prepare data -- #
param$data = param$data %>%
  llply(function(i_data){
    # create status column
    i_data = make_status_from_rule(
      i_data
      , rule = param$classification_columns[[1]] %>% list_to_Rfilter()
    )
    # prepare
    prepare_life_times(
      data = i_data
      , indiv_col = 'CHASNO'
      , obs_time_col = 'VEHMIL'
      , status_col = 'status'
      , limit = 1)
  }, .progress = 'text')

# -- fit models -- #
param$reports = param$data %>% 
  llply(function(i_data)
    multi_surv_reg(i_data
                   , time_col = 'elapsed'
                   , status_col = 'status'
    ), .progress = 'text'
  )

# -- An analysis collection object -- #


param = analysis_collection(param, pareto_columns = c('CPART', 'CtryOp'))
param2$models$`2018-04-01` %>% plot_life_curve()
plot_surv_analys_collection(param, pred_times = c(100,500,1000)*1000)


# ----------------- #
# light version
# ----------------- #

param_light = analysis_collection(param)
# debugonce(light_flexsurvreg)
# param_light$models = param_light$models['2017-02-01']
param_light = s_analysis_collection(param_light)
param_light$surv_predictions = NULL
class(param_light)

# ---------------------------------- #

tf = Sys.time()
tf - ti

debugonce(plot_surv_analys_collection)
param_light$models = param_light$models['2017-02-01']
plot_surv_analys_collection(param_light, pred_times = c(50, 100, 150, 200)*1000)
# plot_surv_analys_collection(param, pred_times = c(50, 200, 300, 800))
plot_life_curve(param$models$`2017-06-01`)

# ---------------------------------- #
# simple version to Upload to MongoDb
# ---------------------------------- #

doc_to_insert = param_light
doc_to_insert = simply_analysis(param_light)
doc_to_insert %>% str(1)


# plot_surv_analys_collection(param)
# plot_surv_analys_collection(param_light)
# plot_surv_analys_collection(doc_to_insert)

cowplot::plot_grid(
  plot_surv_analys_collection(param)
  , plot_surv_analys_collection(param_light)
  , plot_surv_analys_collection(doc_to_insert)
  , nrow = 3
  , labels = c('original', 'light', 'simple'))
# ---------------------------------- #

sprintf('
The Simple obj has %s.
The light object has %s.
The original has %s.
The proportional size light/original is %s%s
The proportional size Simple/original is %s%s
        '
        , format(object.size(doc_to_insert), 'Kb')
        , format(object.size(param_light), 'Kb')
        , format(object.size(param), 'Kb')
        , round((object.size(param_light) %>% as.numeric() / 
                  object.size(param) %>% as.numeric()) *100,digits=2)
        , '%'
        , round((object.size(doc_to_insert) %>% as.numeric() / 
                   object.size(param) %>% as.numeric()) *100,digits=3)
        , '%') %>% 
  cat()

doc_to_insert$analysis_period[[1]] %>% class()

doc_to_insert %>% str(1)

# ---------------------------------- #
# Load python functions
# ---------------------------------- #

aux = list.files('/Users/leo/Documents/Estudos/R/my_packages/reliabilitytools/python/'
                 , full.names = TRUE)

python_version = dput(system('which python3', intern = TRUE))
# python_version = '/Users/leo/anaconda3/bin/python'
use_python(python_version, required = T)
source_python(aux[1])
source_python(aux[2])
aux
"analysis_collection_mongo_to_r"
"analysis_collection_r_to_mongo"

# add 'doc_to_insert' to mongodb
analysis_collection_r_to_mongo(
  'doc_to_insert'
  , 'analysis'
  , 'reliability')




