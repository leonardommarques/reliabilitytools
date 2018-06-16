#' convert to year month
#'
#' convert an \code{integer} or \code{character} to year month .
#'
#' @param format: A character string. If not specified, it will try "%Y-%m-%d" then "%Y/%m/%d" on the first non-NA element, and give an error if neither works. Otherwise, the processing is via strptime
#' @param date: An \code{integer} or \code{character}
#' @return A \code{Date}
#' @details Transforms year month to year month day \code{Date} object.
#' @export
#' @examples
#' to_yearmonth(201801)
#' to_yearmonth('2018-11', format = '%Y-%m')

to_yearmonth = function(date, format = '%Y%m'){
  
  ################################################+
  ## transforms year month to year month day \code{Date} object.
  # date: An \code{integer} or \code{character}
  # format: A character string. If not specified, it will try "%Y-%m-%d" then "%Y/%m/%d" on the first non-NA element, and give an error if neither works. Otherwise, the processing is via strptime
  ################################################+
  
  if(class(date) %in% c('integer', 'numeric'))
    date = as.character(date)
  
  # add day
  date = paste(date,'01', sep = '-')
  
  date = as.Date(date, format = paste0(format,'-%d'))
  
  return(date)
  
  
}


