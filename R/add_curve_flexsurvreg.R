


###############+
# make life curve from distribution
###############+


#'
#' Distribution life curve
#'
#' Make life curve from distribution
#'
#' @param log_transform: If true, add the \code{log = TRUE} to the distribution function.
#' @param lower: the \code{lower} argument of the dist functions.
#' @param model_flexsurvreg: an object of class \code{flexsurvreg}.
#' @return A Layer for ggplot2 grafics, from a "stat_function()"
#' @details
#' @export
#' @examples
#'
add_curve_flexsurvreg = function(model_flexsurvreg
                                   , lower = FALSE
                                   , log_transform = FALSE
){
  
  ## get the distrbution function
  m_dist = model_flexsurvreg[['dlist']][['name']]
  # name maping
  if(m_dist == 'weibull.quiet') m_dist =  'weibull'
  if(m_dist == 'plognormal') m_dist =  'lnorm'
  # if(m_dist == 'gengamma.orig') m_dist = 'pgengamma'
  
  ## get the PDF	
  if(m_dist == 'gengamma') dist_fun =  pgengamma
  else if(m_dist == 'genf.orig') dist_fun =  pgenf.orig
  else if(m_dist == 'weibull') dist_fun =  pweibull
  else if(m_dist == 'weibull') dist_fun =  pweibull
  else if(m_dist == 'gamma') dist_fun =  pgamma
  else if(m_dist == 'exp') dist_fun =  pexp
  else if(m_dist == 'llogis') dist_fun =  pllogis
  else if(m_dist == 'lnorm') dist_fun =  plnorm
  else if(m_dist == 'gompertz') dist_fun =  pgompertz
  else if(m_dist == 'plognormal') dist_fun =  pnorm # -- plognormal
  else if(m_dist == 'unif') dist_fun = function(...) 1-punif(...) # when there are no failures in the dataset.
  
  
  ## get the paramters
  pars = get_parameters_flexsurvreg(model_flexsurvreg)	
  pars$lower = lower
  pars$log = log_transform
  
  
  ## make the grob (graphical object) for the distribution curve
  gg_stat_function = stat_function(
    fun = dist_fun 	# The pdf 
    , args = pars		# The parametras + lower = FALSE
    , aes_(col = m_dist)			# name for the legend colour
    , inherit.aes = FALSE
  )
  
  
  # return(gg_stat_function = gg_stat_function)
  return(gg_stat_function)	
  
}


