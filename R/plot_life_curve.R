
###############+
# Plot life curve
###############+

#'
#' Life curve
#'
#' Plot life curve
#'
#' @param line_width
#' @param m_flexsurvreg: An object of class flexsurvreg, or a list of objects 'flexsurvreg'. If a list of models is given, the plot will have all the curves
#' @param title_aux: a complement to the title
#' @param x_label, y_label: labels for Y and X axis
#' @param xlim: limits for x-axis. A blank point is added to to the plot, otherwise the life curves are not fully drawn.if xlim is vector of length 1, this will be the upper limmit.
#' @param ylim: limits for y-axis. if ylim is vector of length 1, this will be the lower limmit
#' @return A ggplot of the life curve.
#' @details
#' @export
#' @examples
#'
#'


plot_life_curve = function(m_flexsurvreg
                                , title_aux = ''
                                , x_label = 'time'
                                , y_label = 'reliability'
                                , ylim = c(0,1)
                                , xlim = NA
                                , line_width = 1.5
                                # , ...
){
  
  # -- Data Validation -- #
  { 
    ## ylim ##
    # ylim must be in the interval (0,1)
    if(all(ylim > 1)) ylim = ylim/100
    # ylim must be a two dimentional vector
    if(length(ylim) >=2) ylim = c(max(ylim), min(ylim))
    # if length(ylim) this will be considererd as the lower limit
    if(length(ylim) == 1 ) ylim = c(ylim, 1)
    
    ## xlim ##
    if(!any(is.na(xlim)))
      # xlim must be a two dimentional vector
      if(length(xlim) >=2) xlim = c(max(xlim), min(xlim))
      # if length(xlim) this will be considererd as the upper limit
      if(length(xlim) == 1 ) xlim = c(0,xlim)
      
  }
  #--------------------------+  
  if('multi_surv_reg' %in% class(m_flexsurvreg)){
    m_flexsurvreg = m_flexsurvreg$model
  }
  #--------------------------+  
  
  
  ## creates another list of models when the input is a list of models
  if(class(m_flexsurvreg) == 'list'){
    m_flexsurvreg_old = m_flexsurvreg
    m_flexsurvreg = m_flexsurvreg[[1]]
  }
  
  # -- KM survival estimates
  survfit.aux = extract_km_estimates_from_flexsurvreg(m_flexsurvreg)
  
  
  # -- make the graphical object
  ggaux = ggplot(survfit.aux, aes(x = time, y = surv))+
    geom_point()+
    coord_cartesian(ylim = ylim) +
    labs(title = paste0('Life Curve ', title_aux)
         , y = y_label
         , x = x_label
         , col = '')+
    # - constants from the script 
    scale_x_LIFECURVE +
    scale_y_LIFECURVE +
    theme_LIFECURVE
  
  # -- xlimit for the plot
  if(!any(is.na(xlim))){
    # a point needs to be added because stat_function(...) draw the line up to the higher x value
    ggaux = ggaux + 
      geom_point(aes(x=max(xlim), y=max(xlim)*0), alpha = 0) + 
      coord_cartesian(ylim = ylim, xlim = xlim)
    # ggaux = ggaux + geom_point(x = -Inf, y = -Inf, alpha = 0)
    
  }
  
  
  # -- print
  ggaux = ggaux + add_curve_flexsurvreg(m_flexsurvreg)
  
  # -- add all the other curves when the input is a list of models
  if(exists('m_flexsurvreg_old')){
    for(i in 1:length(m_flexsurvreg_old)){
      ggaux = ggaux + add_curve_flexsurvreg(m_flexsurvreg_old[[i]]) 		
    }
    # -- reorder legend to best curve -- +
    # dist_names = m_flexsurvreg_old %>% map('dlist') %>% map('name') %>% unlist()
    # ggaux = ggaux + scale_color_discrete(breaks = dist_names)
  }
  
  # gg_curve = ggaux
  return(ggaux)
  
}

