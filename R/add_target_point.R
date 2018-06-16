
####################+
# add target points
####################+

#'
#' Target point
#'
#' Add target point and estimated survival of the models for the givem target. 
#'
#' @param adj_axis: if \code{TRUE}, plot will be adjusted to show all estimated survival for the models.
#' @param failure_rate_plot: if \code{TRUE}, the estimates of failure rate (1-survival) will be ploted instead. This is usefull for weibull paper, mostly.
#' @param ggaux: a \code{ggplot} object.
#' @param legend_text: If not specified, the legend will be in the format "surv_target@time_target"
#' @param models_list: An object or a \code{list} of objects returned from code{multi_surv_reg()}
#' @param time_target, surv_target: The target 'time' and 'survival'
#' @param p_size, p_shape, t_size: point size, point shape and text size.
#' @return A \code{ggplot}
#' @details
#' @export
#' @examples
#'

add_target_point = function(ggaux
                              , models_list
                              , time_target = 0
                              , surv_target = 1
                              , legend_text = paste0('Target: ',surv_target*100,'@',time_target)
                              , p_size = 5
                              , p_shape = 18
                              , t_size = 3
                              , adj_axis = TRUE
                              , failure_rate_plot = FALSE
){
  
  
  # -- DATA VALIDATION -- #
  # -- Identifying if it is a curve object (resulting form multi_surv_reg) 
  # -- or a list of curve objects
  if('models' %in% names(models_list)){ # curve object.
    models_list = list(models_list)
  } else if('models' %in% names(models_list[[1]])){ # list of curve objects.
    list_of_curves = TRUE
  } else {
    stop('Make sure "models_list" is a valid object.')
  }
  
  
  # -------------------------- #
  # -- add points and force the parameters to be literal/identity
  # -------------------------- #
  if(failure_rate_plot){
    surv_target = 1 - surv_target
  }
  
  ggaux = ggaux + geom_point(aes_(x = time_target
                                  , y = surv_target
                                  , col = legend_text
                                  , size = p_size
                                  , shape = p_shape))+
    # - force shape and size to be literal/identity
    scale_shape_identity() +
    scale_size_identity()
  
  # -------------------------- #
  # -- data frame with the texts
  # -------------------------- #
  aux = models_list %>%
    map(predict_best_model, pred_times = time_target) %>%
    map(data.frame) %>% rbind_list() %>% data.frame()
  
  if(failure_rate_plot){
    aux$est = 1 - aux$est
  }
  aux$text = as.character(round(aux$est,3)*100)
  
  # -- text layer
  ggaux = ggaux + geom_text(data = aux
                            ,aes(x = time
                                 , y = est
                                 , label = text)
                            , size = t_size
                            , inherit.aes = FALSE)
  
  
  # -------------------------- #
  # -- axis adjust
  # -------------------------- #
  if(adj_axis){
    if(failure_rate_plot){
      ggaux = ggaux + coord_cartesian(ylim = c(10e-7, max(c(aux$est,surv_target))))
    } else {
      ggaux = ggaux + coord_cartesian(ylim = c(min(c(aux$est,surv_target)), 1)
                                      , xlim = c(0, max(aux$time)))
      
    }
  }
  
  # -------------------------- #
  # -- change yaxis format to %
  # -------------------------- #
  # if(failure_rate_plot)
  # ggaux = ggaux + scale_y_continuous(labels=percent)
  
  return(ggaux)
  
}
