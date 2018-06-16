
#' Get distribution parameters
#'
#' get parameters from model 'flexsurvreg'
#'
#' @param model_flexsurvreg: an object of class 'flexsurvreg'.
#' @param name: \code{logical} indicating whether to return name of the distribution or not.
#' @return 
#' @details get parameters from model 'flexsurvreg'
#' @export
#' @examples
#'

get_parameters_flexsurvreg = function(model_flexsurvreg, name = FALSE){
  
  pars_aux = as.list(data.frame(t(data.frame(model_flexsurvreg[['res']][,'est']))))
  names(pars_aux) = model_flexsurvreg[['dlist']][['pars']]
  
  if(name){
    pars_aux$name = model_flexsurvreg[['dlist']][['name']]
  }
  
  return(pars_aux)
  
}

