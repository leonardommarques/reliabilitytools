#'
#' RM prediction curves
#'
#' Plot RM prediction curves from a data.frame ####
#'
#' @param aux_title: A title complement.
#' @param col_legend: a title for the legend.
#' @param ic_area: \code{logical} add confidence interval \code{ribbon} of the max time (lowest line).
#' @param multi_surv_reg: A \code{multi_surv_reg object}
#' @param time_scale: a number to devide the time so it rescales and the legend looks better
#' @param x_grid_step: The spacing between each x grid line.
#' @param ylim: y axix limits
#' @return A ggplot of the RM  prediction data.frame
#' @details
#' @export
#' @examples
#'

plot_predict_multi_surv_reg = function(multi_surv_reg
                                       , time_scale = 1
                                       , col_legend = 'time'
                                       , aux_title = ''
                                       , aux_subtitle = ''
                                       , ylim = c(0,1.0005)
                                       , x_grid_step = 50000
                                       , ic_area = TRUE
){
  
  #--------------------------+
  
  if(!('data.frame' %in% class(multi_surv_reg)))
    predict_df = predict_best_model_multi_surv_reg(multi_surv_reg,
                                                     conf_int = ic_area)
  else predict_df = multi_surv_reg
  
  
  #---+
  # creates an ordered factor column for YearMon
  #---+
  predict_df = predict_df %>%
    mutate(mes = as.character(month)) %>%
    arrange(mes)
  
  predict_df$mes = factor(predict_df$mes
                          , levels = unique(predict_df$mes)
                          , ordered = TRUE
  )  
  levels(predict_df$mes) = format(as.Date(levels(predict_df$mes)), format = "%b%y")
  
  #---+
  # rescales the time
  #---+
  predict_df$time = predict_df$time/time_scale
  predict_df$time = factor(predict_df$time, levels = sort(unique(predict_df$time)), ordered = TRUE)
  
  
  # -- Predictions for the maximum time -- #
  max_predict_df = data.frame(predict_df)
  max_predict_df$time = as.numeric(as.character(max_predict_df$time))
  max_predict_df = predict_df[max_predict_df$time == max(max_predict_df$time),]
  max_predict_df$est = round(max_predict_df$est,3)
  max_predict_df = data.frame(max_predict_df)
  
  #---+
  # plot
  #---+
  ggaux = ggplot(predict_df, aes(x = mes
                                 , y = est
                                 , group = time
                                 # , col = time
  ))+
    geom_line(size = 1, aes(col = time)) + 
    geom_point(size = 1, aes(col = time)) + 
    labs(title = paste0('',aux_title, collapse = '')
         , subtitle = aux_subtitle
         , y = 'Reliability (%)'
         , x = ''
         , col = col_legend)+
    geom_text(data = max_predict_df
              ,aes(x= mes, y = est-0.001, label =  as.character(est*100))
              , size = 2.5)+
    coord_cartesian(ylim = ylim) +
    scale_y_LIFECURVE + # a 'constant' in the script
    theme_MASTER +# a 'constant' in the script
    theme(legend.position="bottom")
    
  
  
  # ------ +
  # Add ribbon for the confidence interval
  # ------ +
  
  if(ic_area){
    ggaux = ggaux+
      geom_ribbon(data = predict_df %>%
                    mutate(lcl = lcl*ifelse(time == max(time), 1, NA),
                           ucl = ucl*ifelse(time == max(time), 1, NA)
                    ),
                  aes(x = mes,
                      ymin = lcl,
                      ymax = ucl,
                      fill = time
                  ),
                  alpha = 0.15,
                  show.legend = FALSE
      )+
      scale_alpha(guide = 'none')
  }
  # ------ +
  
  
  
  return(ggaux)
  
}
