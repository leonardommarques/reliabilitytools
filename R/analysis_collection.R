
#'
#' \code{analysis_collection}
#'
#' define class analysis_collection
#'
#' @param pareto_columns Columns of \code{report[...]$data} to make pareto table. 
#' @param report an \code{object} with the compatible fields to be converted into an analysis_collection.
#' @return An object of class \code{analysis_collection}
#' @details The \cod{report} must have the folloing fields
#'  - \code{reports}: a named \code{list} of \code{multi_surv_reg} objects.
#'  - \code{data}: a named \code{list} containing a collection of \code{data.frame}
#' 
#' @export
#' @examples
#'

analysis_collection = function(
  report
  , pareto_columns = c('CPART', 'DEBCOD')
  
){
  
  report$models = report$reports %>% map('models')
  report$ff_table = report$reports %>% map('ff_table')
  report$pareto = report$data %>%
    map(pareto, columns = pareto_columns)
  
  
  class(report) = c('analysis_collection', 'list')
  return(report)
  
}

