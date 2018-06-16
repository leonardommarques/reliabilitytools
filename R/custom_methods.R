######################+
# methods for NULL and zero failure model
######################+


# ------------------ +
# flexsurvreg_NULL
# this class is used when the data set has no failures.
# ------------------ +

summary.flexsurvreg_NULL = function(x, t=0, ci=FALSE, ...){
  
  # NA dataframe
  result = data.frame(
    time=t,
    est = NA,
    lcl = NA,
    ucl = NA
  )
  
  if(!ci){
    result$lcl = NULL
    result$ucl = NULL
  }
  
  return(result)
  
}

# ------------------ +
# flexsurvreg_faultless
# this class is used when the data set has no failures.
# ------------------ +

summary.flexsurvreg_faultless = function(x, t=0, ci=FALSE, ...){
  # NA dataframe
  result = data.frame(
    time=t,
    est = 1,
    lcl = 1,
    ucl = 1
  )
  
  if(!ci){
    result$lcl = NULL
    result$ucl = NULL
  }
  
  return(result)
  
}


