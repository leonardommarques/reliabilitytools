###############+
# makes an '&' filter statement
###############+

#'
#' & filter statement
#'
#' makes an '&' filter statement
#'
#' @param field_name: the name of the field in the data base
#' @param values: the values to be filtered
#' @return a \code{character} containing the statement.
#' @details
#' @export
#' @examples 
#' vector_to_Rfilter(field_name = 'country',
#' values = c('BRA', 'GER', 'JPN'))

vector_to_Rfilter = function(field_name = ''
                             , values = c('') ){
  
  if(identical(values, '') | all(is.na(values))){
    
    return('')
    
  } else if(length(values) == 1) {
    
    # Vector with the values
    values_vector = strsplit(as.character(values)
                             , split = "[-;]")[[1]]
    
    # filter string
    result = paste0('& '
                    , field_name,  " %in% c('"
                    , paste(values_vector, collapse="', '")
                    ,"') ")
    return(result)
    
  } else {
    values_vector = values
    result = paste0('& '
                    , field_name,  " %in% c('"
                    , paste(values_vector, collapse="', '")
                    ,"') ")
    return(result)
  }
}