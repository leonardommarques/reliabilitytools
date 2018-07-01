
###############################+
# RMx SQLite3 ####
# Separates the data to make the RMx of the given month.
###############################+


#'
#' Title of the function
#'
#' Description of the function
#'
#' @param assemb_col, repair_col: assembly and repair columns .
#' @param current_month: a \code{character} object of Month of the given RM.
#' @param data_base: A string containing the SQLite3 data_base
#' @param population_filter: A filter "WHERE" statement.
#' @param RM_size: the size, in momths, of the RM. Typically 6, 9, 12, 15, 18, 21 or 24 
#' @param select_statement: A \code{character} containing a "SELECT" statement.
#' @return A \code{tibble}
#' @details Makes the RMx data set of the given month.
#' @export
#' @examples
#'

rm_data_separate_SQlite = function(
  data_base = ""
  , RM_size = 6
  , current_month
  , population_filter = NA
  , select_statement = NA
  , assemb_col = "assembly_date"
  , repair_col = "failure_date"
){
  ################################################ +
  ## Makes the RMx data set of the given month. The fields are limited to VdBfqis style of population segregation.Returns a tibble.
  # data_base: A string containing the SQLite3 data_base
  # RM_size: the size, in momths, of the RM. Typically 6, 9, 12, 15, 18, 21 or 24 
  # current_month: a \code{character} object of Month of the given RM.
  # assemb_col, repair_col: assembly and repair coluns 
  ################################################ +
  
  current_month = as.Date(current_month) 
  
  # -- Tranformimg date to '%Y%m' format.
  RM_size = as.integer(str_extract(RM_size, patt = '[0-9]+'))
  repair_upp = as.integer(format(current_month, '%Y%m'))
  repair_lwr = current_month %m-% months(RM_size)
  repair_lwr = as.integer(format(repair_lwr, '%Y%m'))
  
  assemb_upp = current_month %m-% months(1) # Angelo's RM method disconsiders one month of the assembly month
  assemb_upp = as.integer(format(assemb_upp, '%Y%m'))
  assemb_lwr = current_month %m-% months(RM_size)
  assemb_lwr = as.integer(format(assemb_lwr, '%Y%m'))
  
  aux_query = paste(select_statement
                    # Date Filters
                    # ,'\nwhere   1 > 0\n'
                    , sprintf(' \n--------- Date Filters ---------
                              and   %s >= %s
                              and   %s <= %s
                              and   %s >= %s
                              and   %s <= %s'
                              , repair_col, repair_lwr
                              , repair_col, repair_upp
                              , assemb_col, assemb_lwr
                              , assemb_col, assemb_upp
                    )
                    # ,'\nand'
                    , population_filter
                    
  )
  
  
  # print(cat(aux_query))
  # print(cat('\n\n\n\n\n\n\n'))
  
  # retreive the Data.
  idata = sqldf(aux_query, dbname = data_base)
  
  attr(idata,'query') = aux_query
  
  return(idata)
  
  }
