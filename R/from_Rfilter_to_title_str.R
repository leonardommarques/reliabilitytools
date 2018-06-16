
###############################+
# make title string for ggplot
###############################+

#'
#' Title of the function
#'
#' converts an R filter \code{character} to a more human readable text
#'
#' @param rule: A \code{character} filter rule.
#' @return a \code{character}
#' @details
#' @export
#' @examples 
#' from_Rfilter_to_title_str('country %in% c("BRA", "GER", "JPN") & age >= 30')
#'

from_Rfilter_to_title_str = function(rule){
  # --------------- +
  # converts an R filter string to an human readable text
  # --------------- +
  rule %>%
    gsub(patt = '\\"', repl = '') %>%
    gsub(patt = ' %in% ', repl = ':') %>%
    gsub(patt = 'c\\(', repl = '\\(') %>%
    gsub(patt = ',', repl = ';') %>%
    gsub(patt = "\\'", repl = '')
  
}


