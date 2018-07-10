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

light_flexsurvreg = function(model_flexsurvreg
                              # , callable = FALSE
                              ){
  
  if(class(model_flexsurvreg[[1]]) == 'flexsurvreg_faultless'){
    dist = 'punif'
    , parameters = list(min = 0, max = 0)
  } else {
    # distribution
    dist = model_flexsurvreg$dlist$name
    dist = gsub(dist, patt = '\\.quiet', repl='')
    
    # parameters
    parameters = model_flexsurvreg$res[,'est'] %>% as.list()
    names(parameters) = row.names(model_flexsurvreg$res)
  }
  
  surv_function = list(
    dist = dist
    , pars = parameters
  )
  
  surv_function = list(
    dist = dist
    , pars = parameters
  )
  
  # if(callable){
  #   # TO DO : Tranform the function in to a callable mode.
  #   print('callable option not available yet')
  # }
  
  class(surv_function)= c("light_flexsurvreg", 'list')
  
  return(surv_function)

}


summary.light_flexsurvreg = function(object
                                  # , newdata=NULL
                                  # , X=NULL
                                  , type="survival"
                                  # , fn=NULL
                                  , t=NULL
                                  , quantiles=0.5
                                  # , start=0
                                  # , ci=TRUE
                                  # , B=1000
                                  # , cl=0.95
                                  # , tidy=FALSE,
                                  , ...
){
  
  quantiles = t
  parameters = list(x=t, lower=FALSE)
  parameters = c(parameters, object$pars)
  
  if(type == 'survival'){
    fun_prefix = 'p'
    names(parameters)[1] = 'q'
    result_col_name = 'time'
  } else if(type == 'quantile'){
    fun_prefix = 'q'
    names(parameters)[1] = 'p'
    result_col_name = 'quantile'
  }
  
  aux_fun = paste0(fun_prefix, object$dist)
  
  result = do.call(aux_fun, parameters)
  result = data.frame(t, est = result)
  names(result)[1] = result_col_name
  
  return(result)
  
}








