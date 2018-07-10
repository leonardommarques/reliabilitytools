
#'
#' Pareto Table
#'
#' A Pareto table \code{data.frame}
#'
#' @param columns columns of the \code{i_data} to make a pareto.
#' @param i_data a \code{data.frame}
#' @param status_column column to filter events (\code{status == 1})
#' @return A pareto \code{data.frame} for each specified \cdode{columns} contining frequencies (\code{n}) and percentages (\code{prop}). 
#' If \code{status_column} is provided, then \code{i_data} is filtered before making the pareto.
#' @details
#' @export
#' @examples pareto(iris, 'Species', status_column = '')
#'

pareto = function(i_data,columns = c('CPART', 'DEBCOD'), status_column = 'status'){
  paretos = columns
  names(paretos) = paretos
  paretos = map(paretos, function(column){
    
    if(!is.na(status_column) & (status_column != ''))
      i_data = i_data %>% filter(status==1) 
    
    if(nrow(i_data) == 0){
      result = data.frame(0,0,0)
      names(result) = c(column, 'prop', 'n')
      return(result)
    } 
    
    result = i_data %>%
      dplyr::group_by_(column) %>%
      dplyr::summarise(n = n()) %>%
      mutate(prop = paste0(round(100 * n/sum(n), 0), "%")) %>%
      arrange(-n)
    
    return(result)
  })
  return(paretos)
}
