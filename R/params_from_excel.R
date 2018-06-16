#'
#' Paramenters from excel
#'
#' create paramenters from excel table
#'
#' @param classification_columns: : \code{character} vector containing the names of the columns tha will be used in the classification (failure/suspenstion)
#' @param end_period: upper year month limit for the RM.
#' @param file_path: Path to the excell file containing the parameters
#' @param filter_columns: \code{character} vector containing the names of the columns that will be used to filter the population.
#' @param sheet: sheet to read from.
#' @param ...: further arguments for \code(read_excel()).
#' @return 
#' @details
#' makes a list containing the parameters for the RM
#' This function was coded to optimize the RM reports generation.
#' The algorithm for RM is described below:
#'     1) Separate the population in risk
#'     2) Classify the statuses as 'event' or 'suspension'
#'     3) Prepare times (calculate elapsed times)
#'     4) Estimate life curves.
#' 
#' Often, one wants to estimate several curves for different failures causes for the same 
#' Population Under Risk (PUR). So there is no need to separate the population every time,
#' e.g  For the population market_type = FH, the Step 1) needs be executed only once.
#' Once the PUR data is acquired, one can estimate life curves for 'Turbo' , 
#' 'Gear Box' and 'Valves'.
#' @export
#' @examples
#'

params_from_excel = function(file_path =''
                             , sheet = 'params'
                             , end_period = as.Date(Sys.time())
                             , classification_columns = NA
                             , filter_columns = NA
                             , ...){
  
  
  #--------------+
  # read the sheet
  #--------------+
  param_sheet = read_excel(file_path
                           , sheet = sheet
                           , ...)
  
  # ---- +
  # get list of parameters that will be used to classify the status and to filter  population
  # ---- +
  # index of the columns that separate filters and classificators
  # rm_size and params_description must be the limits of this interval
  clean_paramns_columns = tolower(names(param_sheet)) # names(janitor::clean_names(param_sheet[1,]))
  index = which(clean_paramns_columns %in% c('__rm_size__','__analysis_period__', '__params_description__'))
  names(param_sheet)[index] = clean_paramns_columns[index]
  
  # get list of parameters that will be used as classificators
  classification_columns = names(param_sheet)[1:(min(index)-1)]
  
  # get list of parameters that will be used as filters
  filter_columns = names(param_sheet)[(max(index)+1):length(names(param_sheet))]
  
  
  #--------------+
  # Separate the populations
  #--------------+
  populations = split(data.table(param_sheet)
                      , by = c('__rm_size__', filter_columns)) # %>%  map(function(xx) unique(xx, by = c('rm_size', filter_columns)))
  
  
  # -- Parameters creation -- #
  aux_paramns = llply(
    populations
    , function(i_pop){
      
      # browser()
      
      i_pop = as.data.frame(i_pop)
      
      
      params = list(
        # -- RM parameter -- #
        rm_size = unique(i_pop[,'__rm_size__']) #as.numeric(str_extract(unique(i_pop[,'__rm_size__']),patt = '[0-9]+'))
        
        # -- The interval period to be analysed -- #
        , analysis_period = c(as.Date(unique(i_pop[,'__analysis_period__'])), end_period) %>% unique()
        
        
        
        # -- Population under investigation -- #
        , filter_columns = i_pop[filter_columns] %>% 
          unique() %>% 
          janitor::remove_empty_cols() %>%
          as.list() %>% 
          map(function(values) strsplit(as.character(values), split = "[-;]")[[1]])
        
        # , filter_rule_str = i_pop[filter_columns] %>% 
        #   unique() %>% make_where_statement_string() %>%
        #   paste0(collapse = '')
        
        
        # -- rules that will be used to classify an observation as a failure -- #
        , classification_columns = i_pop[classification_columns] %>% 
          data.table() %>%
          split(by = classification_columns) %>%
          map(janitor::remove_empty_cols) %>%
          map(as.list) %>% 
          map(function(xx) map(xx,function(values) strsplit(as.character(values), split = "[-;]")[[1]]))
        
        
        # , classification_rule_str = i_pop[classification_columns] %>% 
        #   data.table() %>%
        #   split(by = classification_columns) %>%
        #   map(make_classification_string) %>%
        #   unname()
        
        # , analysis_alias = i_pop[,'__params_description__'] %>% 
        #   split(by = '__params_description__')
      )
      
      names(params$classification_columns) = i_pop[,'__params_description__'] 
      # names(params$classification_rule_str) = i_pop[,'__params_description__'] 
      
      return(params)
      
      
    }, .progress = 'text'
  )
  
  # rename populations
  rm_names = aux_paramns %>% map('rm_size') %>% unlist() %>% unname()
  
  filters_names = aux_paramns %>% 
    map('filter_columns') %>% 
    map(function(xx) map(xx, function(yy) paste0(yy, collapse = '-'))) %>%
    map(paste0, collapse = ' ')
  
  filters_names = filters_names %>% unlist() %>% unname()
  
  
  pop_names = paste(rm_names, filters_names)
  names(aux_paramns) = pop_names
  
  return(aux_paramns)
  
}

