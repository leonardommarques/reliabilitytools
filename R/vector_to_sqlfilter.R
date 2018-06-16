#' AND SQL filter 
#' makes an 'AND' filter statement
#' 
#'
#' @param field_name The name of the fild in the data base.
#' @param values The values to be filtered
#' @return A \code{character} containing the SQL statement.
#' @details
#' @export
#' @examples
#' vector_to_sqlfilter(field_name = 'country',
#' values = c('BRA', 'GER', 'JPN'))
#'
vector_to_sqlfilter = function(field_name = ''
                               , values = c('') ){
  
  # empty string and NA treament.
  if(identical(gsub(values, patt=' +', repl = ''),'') | (sum(!is.na(values)==0) )){
    
    return('')
    
  } else if(length(values) == 1){
    
    return(paste0('\nand ', field_name, ' in (','"'
                  ,paste(strsplit(as.character(values)
                                  , split = "[-;]")[[1]]
                         , collapse='", "')
                  ,'") ')
    )
  } else {
    return(paste0('\nand ', field_name, ' in (','"'
                  ,paste(values, collapse='", "')
                  ,'") ')
    )
  }
  
}
