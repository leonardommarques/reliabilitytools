#'
#' Simple s_analysis_collection
#'
#' simpyfies analysis objects
#'
#' @param rel_analysis A \code{s_analysis_collection}.
#' @return A list object with simplyfied field types
#' @details Turns all dates into strings and removes all unnecessary fields.
#' @export
#' @examples
#'

simplify_analysis = function(rel_analysis){
  
  # -- assert that non necessary objects are not included -- #
  rel_analysis$data_raw = NULL
  rel_analysis$reports = NULL
  rel_analysis$data_mor = NULL
  
  rel_analysis$data = rel_analysis$data %>%
    map(as.list)
  
  rel_analysis$ff_table = rel_analysis$ff_table %>%
    bind_rows(.id = 'month')
  
  #  -- dates to str -- #
  rel_analysis$analysis_period = rel_analysis$analysis_period %>% as.character()
  
  rel_analysis$ff_table = rel_analysis$ff_table %>% 
    map(data.table) %>% 
    bind_rows(.id = 'month')
  
  rel_analysis$surv_predictions = rel_analysis$surv_predictions %>% 
    mutate(month = as.character(month))
  
  
  return(rel_analysis)
  
}

