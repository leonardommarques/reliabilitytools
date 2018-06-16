#'
#' Title of the function
#'
#' makes an SQL interval filter statement
#'
#' @param field_name: the name of the field in the data base
#' @param values: the values to be filtered
#' @return A \code{character} containing the statement.
#' @details A filter statement for interval limit.
#' @export
#' @examples
#' interval_to_SQLfilter(field_name = 'age', c('>=18','<20'))
#'
interval_to_SQLfilter = function(field_name = ''
                                 , values = c('') ){
  
  values = gsub(values, patt = '<', repl = '< ')
  values = gsub(values, patt = '>', repl = '> ')
  values = gsub(values, patt = '< =', repl = '<= ')
  values = gsub(values, patt = '> =', repl = '>= ')
  
  if(str_count(values, patt = '[<>]') %>% sum() != length(values))
    stop('invalid values for interval.')
  
  values = paste('and ',field_name, values, collapse = ' ')
  
  return(values)
  
}
