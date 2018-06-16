
#'
#' Rule string
#'
#' make rule string for dplyr::filter() taking from list
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
#' list_to_Rfilter(rules)
#' 

list_to_Rfilter = function(rules_list = list(rule1 = 1:3, rule2 = '>2')){
  # ------------------------------ +
  # transforma a named \code{list} into a filter string
  # ------------------------------ +
  # rules_list: names list containing the values of the filter.
  # ------------------------------ +
  
  index = grepl('[><]',rules_list)
  
  var_names = names(rules_list)
  
  # "value %in%"  filter
  rules_list[!index] = llply(seq_along(var_names)[!index], function(ii){
    vector_to_Rfilter(var_names[ii], rules_list[[ii]])
    
  })
  
  # interval filter
  rules_list[index] = llply(seq_along(var_names)[index], function(ii){
    interval_to_Rfilter(var_names[ii], rules_list[[ii]])
  })
  
  # ----- remove the first "&"----- +
  rules_list[[1]] = gsub(rules_list[[1]], patt = '^& ', repl = '')
  rules_list = paste0(rules_list, collapse = '')
  
  
  return(rules_list)
  
}


