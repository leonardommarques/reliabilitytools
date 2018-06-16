


###############################+
# Predicts the survival/reliability of the given times for the model on a multi_surv_reg list ( a list )
###############################+

#'
#' Bulk best Model predict
#'
#' Predicts the survival/reliability of the given times for the model on a multi_surv_reg \code{list}
#'
#' @param best_model: The rank of model to be used in the plot. If best_model = 2, the second best model will be used in the plot. Can be the name of the distribution of the model
#' @param conf_int: if true then the confidence intervals will be calculated.
#' @param multi_surv_reg: a named list containing at least the alement "models" that has a list of "flexsurvreg" models
#' @param pred_times
#' @return A \code{data.frame} with the predicted survival/reliability for  range of life times (resulting from a \code{multi_surv_reg()})
#' @details
#' @export
#' @examples
#'

predict_best_model_multi_surv_reg = function(multi_surv_reg
                                               , pred_times = c(50,100,120,150,200)*1000
                                               , conf_int = FALSE
                                               , best_model = 1
){
  #--------------------------+
  # Returns a data.frame with the predicted survival/reliability for  range of life times (resulting from a f.multi.surv.reg() )
  # multi_surv_reg: a named list containing at least the alement "models" that has a list of "flexsurvreg" models
  # pred_times: the list of times to predict to.
  # conf_int
  #--------------------------+
  
  #########+
  # Makes the prediction 
  #########+
  predict_list = llply(multi_surv_reg, predict_best_model
                       , pred_times=pred_times,type="survival"
                       , conf_int=conf_int
                       , best_model = best_model)
  
  #########+
  # Adds the name of the items as the "month" column.
  #########+ 
  
  if(is.null(names(predict_list))){
    names(predict_list) = 1:length(predict_list)
  }
  
  
  predict_list = llply(names(predict_list)
                       , function(xx){
                         aux_df = as.data.frame(predict_list[[xx]])
                         aux_df[,'month'] = xx
                         return(aux_df)
                       })
  
  predict_list = rbindlist(predict_list)
  
  return(predict_list)
  
}
