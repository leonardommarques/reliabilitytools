#'
#' Status Rule
#'
#' Make status column from a given rule
#'
#' @param dfr: a \code{data.frame}
#' @param rule: A \code{character} (or a syntactically valid \code{R} expression_ containing the expresion that will determine if an observation is a failure.
#' @return 
#' @detailsAdd a column containing the status (numerical) of the observation according to a rule.
#' @export
#' @examples
#'
make_status_from_rule <- function(dfr, rule){
  
  # eval(substitute(rule), dfr) 
  if(class(rule) == 'character')
    aux = eval(parse(text = rule), dfr)
  else 
    # aux = eval(substitute(rule), dfr) 
    print('Not a character expresion')	
  
  dfr$status = as.numeric(aux)
  return(dfr)
}
