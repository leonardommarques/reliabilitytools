

###############+
# makes an interval filter statement
###############+

#'
#' Interval R filter
#'
#' makes an interval filter statement
#'
#' @param field_name: the name of the field in the data base
#' @param values: the interval velues to be filtered
#' @return: A \code{character} containing the interval filter statement.
#' @details Makes a filter statement for interval limit.
#' @export
#' @examples
#' interval_to_Rfilter(field_name = 'age', c('>=18','<=20'))
#'

interval_to_Rfilter = function(field_name = ''
                               , values = c('') ){
  
  values = gsub(values, patt = '<', repl = '< ')
  values = gsub(values, patt = '>', repl = '> ')
  values = gsub(values, patt = '< =', repl = '<= ')
  values = gsub(values, patt = '> =', repl = '>= ')
  
  # validation
  if(str_count(values, patt = '[<,>]') %>% sum() != length(values))
    stop('invalid values for interval.')
  
  
  values = paste('& ',field_name, values, collapse = ' ')
  
  return(values)
  
}
