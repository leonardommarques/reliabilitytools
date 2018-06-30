
#'
#' Best Life curve
#'
#' Plot life curve from best model
#'
#' @param aux_title: Complement to title
#' @param best_model: The rank of model to be used in the plot. If best_model = 2, the second best model will be used in the plot
#' @param conf_int: Add confidence interval if \code{TRUE}
#' @param surv_analys_collection: An object of class \code{surv_analys_collection}
#' @param pred_times: Prediction times.
#' @param time_scale: a number to devide the time so it rescales and the legend looks better
#' @param ylim: Y axis limits
#' @return A ggplot RM life curve of the best model.
#' @details
#' @export
#' @examples
#'

plot_surv_analys_collection = function(surv_analys_collection
                                       , pred_times = c(50, 100, 120, 150, 200) * 1000
                                       , conf_int = FALSE
                                       , aux_title = ''
                                       # , aux_subtitle = ''
                                       , ylim = c(0,1)
                                       , time_scale = 1000
                                       , best_model = 1
                                       , add_ff_table = FALSE
){
  
  
  
  # -- preprocessing -- +
  aux_title = sprintf('RM%i - %s'
                      , surv_analys_collection$rm_size
                      , surv_analys_collection$classification_columns %>% 
                        names)
  
  aux_subtitle = sprintf('Population - %s\nFailure Classification Rule - %s'
                         , surv_analys_collection$filter_columns %>% paste(collapse = ' ')
                         , surv_analys_collection$classification_columns[[1]] %>%
                           list_to_Rfilter() %>%
                           from_Rfilter_to_title_str()
                         
  )
  
  # -- prediction data.frame -- +
  predictions_df = surv_analys_collection$models %>%
    map(function(x){
      summary(x[[best_model]]
              , t = pred_times
              , type = 'survival'
              , ci=conf_int ) %>%
        as.data.table()
    }) %>%
    bind_rows(.id = 'month')
  
  # --- tendecy plot --- +
  ggaux = plot_predict_multi_surv_reg(
    predictions_df
    , aux_title = aux_title
    , aux_subtitle = aux_subtitle
    , time_scale = 1000
    , col_legend = 'time (1,000km)'
    , ylim = ylim
    , ic_area = conf_int
  )
  
  
  if(add_ff_table){
    
    ggaux = add_ff_table(ggaux, multi_surv_reg = surv_analys_collection)
    
  }
  
  
  
  return(ggaux)
  
}
