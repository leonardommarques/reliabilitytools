
#' Prediction table
#'
#' Prediction table
#'
#' @param avg_cost: a \code{numeric} containing the avarage cot per failure
#' @param rel_model: a \code{flexsurvreg} object
#' @param ...: further arguments for predict_best_model()
#' @return A prediction \code{data.table} of a given model
#' @details
#' @export
#' @examples

prediction_table = function(rel_model, avg_cost = NA, ...){
  
  
  predictions = predict_best_model(rel_model
                                     , conf_int = TRUE
                                     , ...) %>% 
    data.frame()
  
  
  predictions$avg_cost = avg_cost
  
  
  if(!is.na(avg_cost)){
    
    predictions = predictions %>% 
      # data.frame() %>%
      mutate(avg_cost = (1-est) * avg_cost) %>%
      mutate(avg_cost = round(avg_cost,2))
    
    names(predictions) = c('KM', 'Reliabiltiy', 'lwr','uppr', 'cost')
    
  } else {
    predictions$avg_cost = NULL
    names(predictions) = c('KM', 'Reliabiltiy', 'lwr','uppr')
  }
  
  
  
  return(predictions)
  
}

