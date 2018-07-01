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

# library(reliabilitytools)
w_ = list.files("/Users/leo/Documents/Estudos/R/my_packages/reliabilitytools/R"
                , full.names = TRUE) %>%
  lapply(source)

# ---------------- #

data_base = '/Users/leo/Documents/Estudos/R/my_packages/reliabilitytools/inst/extdata/example_data.db'

# ---------------- #
aux_query = "
SELECT
* 
, cast( substr(assembly_date,0,5) as INT)*100 + 
cast(substr(assembly_date,6,8) as int) as assembly_int
--cast(substr(assembly_date,6,8) as int)*100 + 
--cast(substr(assembly_date,9,11) as int) as assembly_int

, cast( substr(failure_date,0,5) as INT)*100 + 
cast(substr(failure_date,6,8) as int) as failure_int
--cast(substr(failure_date,6,8) as int)*100 + 
--cast(substr(failure_date,9,11) as int) as failure_int

FROM failures 
where 1>0
"

campos = sqldf(paste(aux_query, 'limit 10'), dbname = data_base)
campos
campos %>% names() %>% sort()

param = list(
  rm_size = 12
  , anaysis_period = c(as.Date('2017-01-01'), as.Date('2017-06-01'))
  , filter_columns = list(model = "A")
  , classification_columns = list( CAUSE_171 = list(failure_cause_group = 171))
)

param$data = seq.Date(param$anaysis_period[1]
                      , param$anaysis_period[2]
                      , by = 'months'
                      ) %>% 
  rev()

names(param$data) = param$data

# -- Get Data -- #
param$data = param$data %>% 
  map(function(cur_date)
    rm_data_separate_SQlite(
      data_base = data_base
      , current_month = cur_date
      , RM_size = param$rm_size
      , select_statement = aux_query
      , population_filter = param$filter_columns %>% list_to_SQLfilter()
      , assemb_col = "assembly_int"
      , repair_col = "failure_int"
    )
  )

param$data %>% str(1)

# -- prepare data -- #
param$data = param$data %>%
  map(function(i_data){
    # create status column
    i_data = make_status_from_rule(
      i_data
      , rule = param$classification_columns[[1]] %>% list_to_Rfilter()
    )
    # prepare
    prepare_life_times(
      data = i_data
      , indiv_col = 'id'
      , obs_time_col = 'cycles'
      , status_col = 'status')
  }
  )

# -- fit models -- #
param$reports = param$data %>% 
  map(function(i_data)
    multi_surv_reg(i_data
                   , time_col = 'elapsed'
                   , status_col = 'status'
                   
    
  )
)


# -- A better object -- #
param$models = param$reports %>% map('models')

# ----------------- #
# light version
# ----------------- #


param_light = param

# -- models -- #
param_light$models = lapply(param_light$models, function(x) lapply(x, light_surv_fun))

plot_surv_analys_collection(param_light, pred_times = c(50, 200, 300, 800))
plot_surv_analys_collection(param, pred_times = c(50, 200, 300, 800))

# -- data -- #
param_light$data_mor = param_light$data %>% 
  bind_rows() %>%
  data.table() %>%
  setDT() %>%
  setorder(id) %>%
  unique(by = c('id'))


param_light$data = param_light$data %>% 
  map(function(i_data){
    i_data %>%
      select(id)
  }
  )

# remove old models
for(i in param_light$report %>% names()){
  print(i)
  param_light$reports[[i]]$models = NULL
}


# --
sprintf('The light object has %s.\nThe original has %s.\nThe proportional size is %s%s'
        , format(object.size(param_light), 'Kb')
        , format(object.size(param), 'Kb')
        , round(object.size(param_light) %>% as.integer()  %>% round(3) / 
          object.size(param) %>% as.integer()*100,digits=1)
        , '%'
        ) %>% cat()



param_light$data[[1]] = param_light$data_mor %>%
  filter_(list(id = param_light$data[[1]]$id) %>%
                                               list_to_Rfilter()
          )
                     

param_light$data[[1]] %>%class()
param$data[[1]] %>% class()

identical(param_light$data[[1]] %>% data.frame(), param$data[[1]])

param_light$data[[1]][[i]] %in% param$data[[1]][[i]] %>% table(useNA = 'always')

param_light$data[[1]]$index %in% param$data[[1]]$index

filter_()

param_light$data[[1]] %>% data.table()

param_light$reports$`2017-06-01` %>% 
  lapply(function(x){
    format(object.size(x), 'Kb')
  })

param_light$reports = lapply(
  param_light$reports, function(x){
    x$models = NULL
  }
  )




