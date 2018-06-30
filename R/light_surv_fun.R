#'
#' Life curve essential parts
#'
#' Make list with the necessary objects for a live curve distribution.
#'
#' @param model_flexsurvreg: an object of class \code{flexsurvreg}.
#' @return list containing the elements of a \code{flexsurvreg} in a lighter form.
#' @details 
#' @export
#' @examples
#'

light_surv_fun = function(model_flexsurvreg
                              # , callable = FALSE
                              ){
  
  # distribution
  dist = model_flexsurvreg$dlist$name
  dist = gsub(dist, patt = '\\.quiet', repl='')
  
  # parameters
  parameters = model_flexsurvreg$res[,'est'] %>% as.list()
  names(parameters) = row.names(model_flexsurvreg$res)
  
  surv_function = list(
    dist = dist
    , pars = parameters
  )
  
  # if(callable){
  #   # TO DO : Tranform the function in to a callable mode.
  #   print('callable option not available yet')
  # }
  
  return(surv_function)

}
