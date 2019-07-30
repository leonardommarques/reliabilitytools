
#'
#' Small \code{analysis_collection} class.
#'
#' create a \code{s_analysis_collection}
#'
#' @param analysis_collection A \code{analysis_collection} object.
#' @param pred_times A vector containing the times to predict survival for.
#' @param index_col A \code{character} containing the inde (primary key) to indentify the observed time.
#' @return A \code{s_analysis_collection} which is a smaller version of the original \code{analysis_collection} object. 
#' @details The \code{s_analysis_collection} has  objects the following changes
#'  - The \code{models} is not a list of \code{flexsurvreg}.
#'  - \code{data_mor} is created as a \code{data.frame} of the unique registers (according to the \code{index_col} column) of all data.frames in \code{data}
#'  - \code{data} only stores the \code{index_col} column, so the whole \code{data.frame} can be retrieved from it.
#'  - \code{index_col} should be a event/failure id.
#' This object is much more convenient to save in data bases.
#' @export
#' @examples


s_analysis_collection = function(
  analysis_collection
  , pred_times = c(50, 100, 150, 200)*1000
  , conf_int = FALSE
  , index_col = 'index'
  ){
  
  # -- prediction data.frames -- #
  if(is.null(analysis_collection$surv_predictions)){
    analysis_collection$surv_predictions = analysis_collection$models %>%
      map(function(x) predict_best_model(x
                                         , pred_times = pred_times
                                         , conf_int = conf_int) %>% 
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
  analysis_collection$data_mor = analysis_collection$data
  
  # remove empty data.frames to avoid errors
  index = analysis_collection$data_mor %>% 
    map(function(xx) dim(xx)[1]) %>%
    unlist() 
  index = names(index[index >0])
  
  analysis_collection$data_mor = analysis_collection$data_mor[index]
  
  # do not try to rbind an empty list
  if(length(analysis_collection$data_mor) > 0){
    
    analysis_collection$data_mor = analysis_collection$data_mor %>%
      rbindlist() %>%
      data.table() %>%
      setDT(key=index_col) %>%
      unique(by = index_col)
    
    analysis_collection$data = analysis_collection$data %>% 
      map(function(i_data){
        i_data %>%
          select_(index_col)
      })
  } else {
    analysis_collection$data_mor  = as.data.frame(analysis_collection$data_mor)
    
  }
  
  # remove old models
  for(i in analysis_collection$report %>% names()){
    analysis_collection$reports[[i]]$models = NULL
  }
  
  # ----------- #
  class(analysis_collection) = c('s_analysis_collection', 'list')
  return(analysis_collection)
  
  
  }




