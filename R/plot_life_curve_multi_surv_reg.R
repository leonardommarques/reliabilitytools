

###############+
# Plot life curve from best model
###############+


#'
#' Best Life curve
#'
#' Plot life curve from best model
#'
#' @param aux_title: Complement to title
#' @param best_model: The rank of model to be used in the plot. If best_model = 2, the second best model will be used in the plot
#' @param conf_int: Add confidence interval if \code{TRUE}
#' @param multi_surv_reg: List containing at least the alement "models" that has a list of \code{flexsurvreg} models.
#' @param pred_times: Prediction times.
#' @param time_scale: a number to devide the time so it rescales and the legend looks better
#' @param ylim: Y axis limits
#' @return A ggplot RM life curve of the best model.
#' @details
#' @export
#' @examples
#'
plot_life_curve_multi_surv_reg = function(multi_surv_reg
                                            , pred_times = c(50, 100, 120, 150, 200) * 1000
                                            , conf_int = FALSE
                                            , aux_title = ''
                                            , ylim = c(0,1)
                                            , time_scale = 1000
                                            , best_model = 1
                                            ){
  
  # in some cases, multi_surv_reg was a list of one element, and this ine element was a list of n elements
  if(length(multi_surv_reg) == 1 & class(multi_surv_reg[[1]]) == 'list')
    multi_surv_reg = multi_surv_reg[[1]]
  
  
  predic_multi_surv_reg = predict_best_model_multi_surv_reg(multi_surv_reg
                                                              , best_model = best_model
                                                              , pred_times = pred_times
                                                              , conf_int = conf_int)    
  
  # --- tendecy plot --- +
  ggaux = plot_predict_multi_surv_reg(predic_multi_surv_reg
                                      , aux_title = aux_title
                                      , time_scale = time_scale
                                      , col_legend = 'mileage (1,000km)'
                                      , ylim = ylim
                                      , ic_area = conf_int
  )
  
  return(ggaux)
  
}


