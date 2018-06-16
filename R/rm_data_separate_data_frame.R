#'
#' RMx data.frame
#'
#' Separates the data to make the RMx of the given month.
#'
#' @param assemb_col, repair_col: assembly and repair coluns 
#' @param current_month: A \code{character} object of Month of the given RM (YYYY-MM-DD).
#' @param data_base: A source \code{data.frame} to take data from.
#' @param population_filter: A filter \code{character} statement.
#' @param RM_size:: the size, in momths, of the RM. Typically 6, 9, 12, 15, 18, 21 or 24 
#' @return A \code{tibble}.
#' @details Makes the RMx data set of the given month
#' @export
#' @examples
#'

rm_data_separate_data_frame = function(
  data_base = NA
  , RM_size = 6
  , current_month
  # ,COUNTRY = c("ARG", "BOL", "BRA", "CHL", "COL", "CRI", "DOM", "ECU", "ESP", "GTM", "JAM", "PAN", "PER", "PRY", "SLV", "URY", "VEN")
  # ,DEBCOD = c(5,13,15,10,18)
  , population_filter = NA
  , assemb_col = "DWASSD_yearmon"
  , repair_col = "FPAYDT_yearmon"
){
  
  current_month = as.Date(current_month) 
  
  # -- Tranformimg date to '%Y%m' format.
  RM_size = as.integer(str_extract(RM_size, patt = '[0-9]+'))
  repair_upp = as.integer(format(current_month, '%Y%m'))
  repair_lwr = current_month %m-% months(RM_size)
  repair_lwr = as.integer(format(repair_lwr, '%Y%m'))
  
  assemb_upp = current_month %m-% months(1)
  assemb_upp = as.integer(format(assemb_upp, '%Y%m'))
  assemb_lwr = current_month %m-% months(RM_size)
  assemb_lwr = as.integer(format(assemb_lwr, '%Y%m'))
  
  aux_query = paste('1 > 0\n'
                    # Date Filters
                    , sprintf('
                              &   %s >= %s
                              &   %s <= %s
                              &   %s >= %s
                              &   %s <= %s'
                              , repair_col, repair_lwr
                              , repair_col, repair_upp
                              , assemb_col, assemb_lwr
                              , assemb_col, assemb_upp
                    )
                    # ,'\nand'
                    , '\n& '
                    , population_filter
                    
  )
  aux_query = gsub(aux_query, patt = '\n', repl = ' ')
  aux_query = gsub(aux_query, patt = ' +', repl = ' ')
  
  
  # -- retreive the Data. -- #
  idata = data_base %>% filter_(aux_query)
  # attr(idata,'query') = aux_query
  return(idata)
  
}