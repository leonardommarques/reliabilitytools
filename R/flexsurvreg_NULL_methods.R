

###############################+
# Summary method for the class "flexsurvreg_NULL"
# This method is needed because when there is no data , the survival
# curve will not be made. And when the script needs to predict the survival for this
# data set, it has to return NA
###############################+

#'
#' Title of the function
#'
#' Prediction method for the flexsurvreg_NULL class
#'
#' @param ...
#' @param ci: confidence interval \code{logical}
#' @param t: time
#' @param x: obj
#' @return 
#' @details This method is necessary because when there is no data , the survival curve will not be made. And when the script needs to predict the survival for this data set, it has to return NA.
#' @export
#' @examples
#'

summary.flexsurvreg_NULL <- function(x,t=1,ci=FALSE,...) { 
  
  time = t
  est = rep(NA,length(t))
  
  predictions = data.frame(time, est)
  
  if(ci==TRUE){
    predictions$lcl = rep(NA,length(t))
    predictions$ucl = rep(NA,length(t))
  }
  
  return(predictions)
}

