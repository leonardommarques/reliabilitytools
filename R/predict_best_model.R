#'
#' Best Fit Prediction
#'
#' Predicts the survival/reliability of the given times for the model
#'
#' @param best_model: The rank of model to be used in the plot. If best_model = 2, the second best model will be used in the plot. Can be the name of the distribution of the model
#' @param conf_int: If \code{TRUE} then the confidence intervals will be calculated.
#' @param m_flexsurvreg: list containing at least the alement "models" that has a list of \code{flexsurvreg} models. Or an object of class \code{flexsurvreg}
#' @param pred_times: the list of times to predict to.
#' @param ...: further argumentrs for \code{summary()}
#' @return 
#' @details Returns a data.frame with the predicted survival/reliability for  range of life times (resulting from a \code{multi_surv_reg()}). If the object is a model itself then the function does not search for the best model.
#' @export
#' @examples
#'

predict_best_model = function(m_flexsurvreg
                                , pred_times = c(50,100,120,150,200)*1000
                                , conf_int = FALSE
                                , best_model = 1
                                , ...
){
  
  if('multi_surv_reg' %in% class(m_flexsurvreg)){
    summary(m_flexsurvreg$models[[best_model]],
            t=pred_times,
            type="survival",
            ci=conf_int
            , ...)
  } else if( any(c('flexsurvreg', 'light_flexsurvreg' ) %in% class(m_flexsurvreg[[best_model]]))) {
    summary(m_flexsurvreg[[best_model]], t=pred_times,type="survival",ci=conf_int, ...)
  } else {
    summary(m_flexsurvreg, t=pred_times,type="survival",ci=conf_int, ...)
  }
  
}
