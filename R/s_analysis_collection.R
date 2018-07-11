
#'
#' Small \code{analysis_collection} class.
#'
#' create a \code{s_analysis_collection}
#'
#' @param analysis_collection A \code{analysis_collection} object.
#' @param pred_times A vector containing the times to predict survival for.
#' @return A \code{s_analysis_collection} which is a smaller version of the original \code{analysis_collection} object. 
#' @details The \code{s_analysis_collection} has lesser objects the following changes
#'  - The \code{models} is not a list of \code{flexsurvreg}.
#'  - \code{data_mor} is created as a \code{data.frame} of the unique registers (according to the \code{index} column) of all data.frames in \code{data}
#'  - \code{data} only stores the \code{index} column, so the whole data.framae can be retrieved from it.
#' This object is much more convenient to save in data bases.
#' @export
#' @examples
#'

s_analysis_collection = function(
  analysis_collection
  , pred_times = c(50, 100, 150, 200)*1000
  , ci = FALSE){
  
  # -- prediction data.frames -- #
  if(is.null(analysis_collection$surv_predictions)){
    analysis_collection$surv_predictions = analysis_collection$models %>%
      map(function(x) predict_best_model(x, pred_times = pred_times, ci = ci) %>% 
          as.data.table()
      ) %>%
      bind_rows(.id = 'month') %>%
      arrange(month)
  }
  
  
  # -- smaller models -- #
  analysis_collection$models = lapply(
    analysis_collection$models
    , function(x) lapply(x, light_flexsurvreg))
  
  # -- smaller data -- #
  analysis_collection$data_mor = analysis_collection$data %>% 
    bind_rows() %>%
    data.table() %>%
    setDT() %>%
    setorder(index) %>%
    unique(by = c('index'))
  
  analysis_collection$data = analysis_collection$data %>% 
    map(function(i_data){
      i_data %>%
        select_('index')
    }
    )
  
  # remove old models
  for(i in analysis_collection$report %>% names()){
    analysis_collection$reports[[i]]$models = NULL
  }
  
  # ----------- #
  class(analysis_collection) = c('s_analysis_collection', 'list')
  return(analysis_collection)
  
  
  }




