#'
#' Reliability summary information
#'
#' A summary of reliability information
#'
#' @param pareto_col: A vector of (\{character}) with the columns to make a pareto.
#' @param reference_month: Reference month.
#' @param rel_analysis: a \code{list} containing the elements \code{data} and \code{models}, which are \code{lists}.
#' @param rel_quantiles: \code{numeric} probabilities to estimate time quantiles for.
#' @return a summary  \code{list} containing the most relevant information of the analysis.
#' @details
#' @export
#' @examples
#'

reliability_summary = function(rel_analysis,
                               reference_month,
                               pareto_col = NA,
                               rel_quantiles = c(0.05, 0.1)){
  
  # --------------------- #
  # parameters validation 
  # --------------------- #
  
  if(!(with(rel_analysis, exists('data') & exists('models'))))
    stop('verify rel_analysis argument.')
  
  # -- quantiles (B)
  if(any(rel_quantiles <0) | any(rel_quantiles>1)){
    warning('rel_quantiles outside the interval [0,1] where igrnored.')
    rel_quantiles = rel_quantiles[rel_quantiles >=0 & rel_quantiles<=1]
  }
  
  # -- reference month
  if(any(class(reference_month) != 'character'))
    stop('invalid reference_month')
  
  # -- pareto_col
  pareto_col = pareto_col[!is.na(pareto_col)]
  if(identical(pareto_col, logical(0))) 
    pareto_col = NA
  # if(!(class(pareto_col) == 'character'))
  
  
  
  # --------------------- #
  
  
  # aux_list = names(rel_analysis$data)
  # names(aux_list) = aux_list
  aux_list = reference_month
  names(aux_list) = reference_month
  
  aux_list = llply(
    aux_list, #names(rel_analysis$data),
    function(i_analysis, index){
      
      # browser()
      # --------------------- #
      # -- failure table -- #
      # --------------------- #
      failures = i_analysis$data[[index]] %>% 
        filter(status == 1)
      
      # --------------------- #
      # -- Average cost -- #
      # --------------------- #
      avg_cost = mean(failures[,'Total_Cost']) %>% round(2)
      
      # --------------------- #
      # -- predict table -- #
      # --------------------- #
      predict_table = prediction_table(i_analysis$models[[index]][[1]],
                                       avg_cost = avg_cost)
      
      # --------------------- #
      # -- failure cause pareto -- #
      # --------------------- #
      if(!is.na(pareto_col)){
        names(pareto_col) = pareto_col
        pareto = pareto_col %>%
          map(function(x){
            
            if(x %in% names(i_analysis$data[[index]]) ){
              i_analysis$data[[index]] %>%
                filter(status == 1) %>%
                select(x) %>%
                table() %>%
                data.frame() %>%
                plyr::rename(replace = c('.'=x, 'Freq'='qnt')) %>%
                mutate(perc = qnt/sum(qnt)
                       ,perc = round(perc, 3)*100
                ) %>%
                arrange(-perc)
            } else {
              pareto = paste(x, ' not present in data set')
            }
          }
          )
        
      } else { pareto = NA }
      
      # --------------------- #
      # -- B5, B10 (...) -- #
      # --------------------- #
      if(i_analysis$models[[index]][[1]] == 'flexsurvreg_NULL'){
        
      } else {
        b_estimates = safely(with, otherwise = NA)(i_analysis$models[[index]][[1]]
                                                   , do.call(dfns$q,
                                                             c(list(p=rel_quantiles),res[,'est'] %>% as.list()) 
                                                   )
        )
        # return error as an attribute when there is one.
        if(is.na(b_estimates$result)){
          b_estimates = b_estimates %>%
            (function(aux){
              result = rep(aux$result, length(rel_quantiles))
              attr(result, 'error') = aux$error
              return(result)
            })
        }
        names(b_estimates) =  paste0('q',rel_quantiles)
      }
      
      # --------------------- #
      # -- distribution parameters -- #
      # --------------------- #
      dist_pars = i_analysis$models[[index]][1] %>% 
        safely(map, otherwise = NA)(get_parameters_flexsurvreg, name = TRUE) %>%
        (function(aux){
          result = rep(aux$result, length(rel_quantiles))
          attr(result, 'error') = aux$error
          return(result)
        })
      
      # browser()
      
      return(list(avg_cost = avg_cost,
                  # failures = failures,
                  predict_table = predict_table,
                  pareto = pareto,
                  b_estimates = b_estimates,
                  dist_pars = dist_pars)
      )
      
      
    },  i_analysis = rel_analysis
  )
  
}

