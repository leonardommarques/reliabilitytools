
###############################+
# make rule string for SQL taking from list
###############################+

#'
#' Title of the function
#'
#' Description of the function
#'
#' @param rules_list: Named \code{list} containing the values of the filter.
#' @return A \code{character} containing the rule.
#' @details
#' @export
#' @examples
#' rules = list(
#'   country = c('BRA', 'GER', 'JPN'),
#'   grade = c('A', 'B', 'C'),
#'   age = c('>10', '<25')
#'   
#' )
#' list_to_SQLfilter(rules)
#'

list_to_SQLfilter = function(rules_list = list(rule1 = 1:3, rule2 = '>2')){
  # ------------------------------ +
  # transforma a named \code{list} into a filter string
  # ------------------------------ +
  # rules_list: names list containing the values of the filter.
  # ------------------------------ +
  
  index = grepl('[><]',rules_list)
  
  var_names = names(rules_list)
  
  # "value %in%"  filter
  rules_list[!index] = llply(seq_along(var_names)[!index], function(ii){
    # vector_to_Rfilter(var_names[ii], rules_list[[ii]])
    vector_to_sqlfilter(var_names[ii], rules_list[[ii]])
    
  })
  
  # interval filter
  rules_list[index] = llply(seq_along(var_names)[index], function(ii){
    # interval_to_Rfilter(var_names[ii], rules_list[[ii]])
    interval_to_SQLfilter(var_names[ii], rules_list[[ii]])
    
  })
  
  # ----- remove the first "and" and paste.----- +
  # rules_list[[1]] = gsub(rules_list[[1]], patt = '^and ', repl = '')
  rules_list = paste0(rules_list, collapse = '')
  
  
  return(rules_list)
  
}



