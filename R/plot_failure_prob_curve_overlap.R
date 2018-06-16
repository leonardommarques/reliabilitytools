
#'
#' Overlaped failure rate curves
#'
#' Plot two or more failure rate curves on top of each other
#'
#' @param add_curves: if true, the distribution curves will be added
#' @param best_model: The rank of model to be used in the plot or the name fo the distribution. If best_model = 2, the second best model will be used in the plot
#' @param group_names: Names for the curves
#' @param models_groups: A \code{list} of objects containing the element 'models' (which is a list of flexsurvreg')
#' @param title_aux: complement to the title
#' @param x_label, y_label:  X and Y axis labels
#' @return A ggplot of the plots overlaped.
#' @details
#' @export
#' @examples
#'

plot_failure_prob_curve_overlap = function(models_groups
                                             , group_names = names(models_groups)
                                             , title_aux = ''
                                             , x_label = 'Mileage(km)'
                                             , y_label = 'Failure Prob.'
                                             , add_curves = TRUE
                                             , best_model = 1){
  #--------------------------+
  # Plot two or more life curves on top of each other
  # return a ggplot of the plots overlaped.
  #--------------------------+
  # models_groups: a list of objects containing the element 'models' (which is a list of flexsurvreg')
  # add_curves: if true, the distribution curves will be added
  # best_model: 
  # best_model: The rank of model to be used in the plot or the name fo the distribution. If best_model = 2, the second best model will be used in the plot
  #--------------------------+
  
  # -- constant breaks -- #
  n = 5
  aux_from = 0.0002
  aux_to   = 0.0010
  aux_breaks = rep(seq(aux_from, aux_to, length.out = n),3)
  aux_breaks = aux_breaks * rep(c(1,10,100), each = n)
  
  # -- add generic names in case models_groups is not named
  if(is.null(names(models_groups)))
    names(models_groups) = as.factor(seq_along(models_groups))
  
  
  # -- Concatenate the name of the best distribution to the name of the group
  # -- and creates the curve grobs (graphical objects)
  if(add_curves){
    
    # -- names
    aux = llply(models_groups, function(model_aux){
      names(model_aux$models[best_model])
      # names(model_aux$models)[best_model]
    })
    
    # -- groups and dist. names concatenation.
    aux = unlist(aux)
    names(models_groups) = paste(names(models_groups), aux, sep = ' - ')
    
    # -- dist curves
    list_aux = names(models_groups)
    names(list_aux) = list_aux
    dist_curves = llply(list_aux, function(models_index){
      aux = add_curve_flexsurvreg(models_groups[[models_index]]$models[[best_model]]
                                    , log_transform = !TRUE
                                    , lower = TRUE)
      aux$mapping$colour = models_index
      return(aux)
    })
    
  }
  
  
  # -- make data.frame with the KM estimates
  km_estimates_df = llply(models_groups, function(model_aux){
    km_df = extract_km_estimates_from_flexsurvreg(model_aux$models)
    
  })
  
  
  # -- insert the names as a column
  aux_list = names(km_estimates_df)
  names(aux_list) = aux_list
  km_estimates_df = llply(aux_list, function(group_name){
    aux_da = km_estimates_df[[group_name]]
    aux_da['Group'] = group_name
    return(aux_da)
    
  })
  
  # -- rbind the dataframes
  # km_estimates_df = do.call(rbind, km_estimates_df)
  km_estimates_df = rbindlist(km_estimates_df)
  
  
  # -- plot points
  ggaux = ggplot(km_estimates_df, aes(x = time, y = 1-surv, col = Group))+
    geom_point()+
    # geom_line()+
    # coord_cartesian(ylim = ylim) +
    labs(title = paste0('Life Curve ', title_aux)
         , y = y_label
         , x = x_label
         , col = '')
  # + theme_MASTER
  
  
  # -- add dist curves
  if(add_curves)
    for(i in dist_curves){
      ggaux = ggaux + i
    }
  
  
  
  # -- Chage the axis scales -- #
  ggaux = ggaux + 
    scale_x_continuous(trans = log_trans()
                       , breaks = trans_breaks(function(x) x/10^3, function(x) x*10^3)
                       , labels = trans_format(function(x) x/10^3
                                               , math_format(.x*k)))+
    scale_y_continuous(trans = log_trans()
                       , breaks = aux_breaks
                       , labels = percent)
  
  # -- end -- #
  return(ggaux)
  
}
