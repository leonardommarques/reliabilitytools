
#'
#' Title of the function
#'
#' Description of the function
#'
#' @param param: a list contining the  fields: "rm_size", "analysis_period", "filter_columns", "classification_columns", "rm_report"
#' @return 
#' @details
#'    -rm_size: the window size of the RM
#'    -analysis_period: vector length two representing the range of the report.
#'    -filter_columns: list of filter columns
#'    -classification_columns: list of classification columns
#'    -rm_report: list containing \code{data} (result from code{prepare_life_times} and \code{fit} (resilt from \code{multi_surv_reg})
#' @export
#' @examples
#'
#'    

param_to_surv_analys_collection = function(param){
  # suv_analys_collection
  
  # ---------------------------- +
  # -- object outer structure check -- +
  components = c("rm_size"
                 , "analysis_period"
                 , "filter_columns"
                 , "classification_columns"
                 # , "data"
                 , "rm_report")
  
  if(!all(components %in% names(param))){
    components = components[!(components %in% names(param))]
    stop('Missing components: ', paste(components, collapse = ', '))
  }
  
  # -- inner structure -- +
  if(!(is.integer(param$rm_size) | is.numeric(param$rm_size)) ) stop('rm_size must be numeric or integer')
  
  if(!all(is.Date(param$analysis_period))) stop('analysis_period must be Date')
  
  if(!all(is.list(param$filter_columns),
          !is.null(names(param$filter_columns))
  )) stop('filter_columns must be a named list')
  
  
  if(!all(is.list(param$filter_columns),
          !is.null(names(param$filter_columns))
  )) stop('filter_columns must be a named list')
  
  
  if(!all(is.list(param$rm_report)
          , !is.null(names(param$rm_report))))
    stop('filter_columns must be a named list')
  # ---------------------------- +
  
  param2 = names(param$rm_report)
  names(param2) = param2
  param2 = llply(
    param2
    , parameter = param 
    ,.fun = function(i_report, parameter){
      
      report = parameter
      
      # -- extract results for a more friendly object -- +
      report$rm_report = report$rm_report[[i_report]]
      report$classification_columns = report$classification_columns[i_report]
      
      # -- data
      report$data = report$rm_report$data
      report$data %>% names
      
      # -- extract fit results
      report$rm_report = report$rm_report$fit
      useful_items = c('models', 'ff_table')
      for(item in useful_items){
        # print(item)
        report[[item]] = report$rm_report %>% map(item)
        # report[[item]] = report$rm_report[[item]]
        
      }
      report$rm_report = NULL
      
      class(report) = 'surv_analys_collection'
      
      return(report)
      
    }
  )
  
  
  return(param2)
  
}


