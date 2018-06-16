

###############################+
# Faz tabela de IN&OUT analysis
###############################+

#' IN & OUT Table 
#'
#' The table for the IN & OUT analysis.
#'
#' @param data: A \code{data.frame}
#' @param no_fail_dist: number of months to concatenate the column containing the distribution of failures along the assembly month. Usefull to see if the recent failures are of individuals recently born.
#' @param reference_month: reference month
#' @param status_col, assembly_col, repair_col: \code{character} status, assembly and repair columns.
#' @param RM_size: RM size.
#' @return 
#' @details
#' @export
#' @examples
#'

in_out_table = function(data, 
                        reference_month,
                        RM_size,
                        status_col = 'status',
                        assembly_col, 
                        repair_col,
                        no_fail_dist = 2
){
  
  aux_fin = as.Date(reference_month)
  aux_ini = aux_fin %m-% months(RM_size)
  in_out  = data.frame(month = seq.Date(aux_ini, aux_fin, by = 'month'))
  in_out  = in_out %>% mutate(month = as.character(month))
  
  # -- rename columns -- #
  aux_replace = c('status', 'assembly', 'repair')
  names(aux_replace) = c(status_col, assembly_col, repair_col)
  data = as.data.frame(data)[,names(aux_replace)]
  data = data %>% plyr::rename(replace = aux_replace)
  
  # -- assert that assembly and repair will return values in table()  even when data has no record -- #
  data = data %>% mutate(assembly = factor(assembly, levels = in_out$month),
                         repair = factor(repair, levels = in_out$month)
  )
  
  
  # -- filter -- #
  data = data[data$status == 1, ]
  
  # -- assembly -- #
  aux_da = table(data$assembly) %>% 
    data.frame() %>%
    plyr::rename(c('Var1'='month','Freq'='assy_month'))
  
  
  in_out = left_join(
    in_out
    , aux_da
    , by = 'month'
  )
  
  # -- repair -- #
  aux_da = table(data$repair) %>% 
    data.frame() %>%
    plyr::rename(c('Var1'='month','Freq'='repair_month'))
  
  in_out = left_join(
    in_out
    , aux_da
    , by = 'month'
  )
  
  
  # -- failure distribution -- #
  # filter months of interest
  index = (length(in_out$month) - no_fail_dist+1):length(in_out$month)
  in_out$month[index]
  
  # data frame contining the distribution of failures
  aux_da = data[data$repair %>% as.character() %in% in_out$month[index], ]
  aux_da = table(aux_da$assembly, aux_da$repair) %>% 
    data.frame()
  names(aux_da) = c('assembly', 'repair', 'Freq')
  
  aux_da = aux_da[aux_da$repair %>% as.character() %in% in_out$month[index], ]
  
  aux_da = dcast(aux_da, assembly ~ repair
                 , value.var = 'Freq' )
  
  
  # -- join -- #
  in_out = left_join(
    in_out
    , aux_da
    , by = c('month' = 'assembly')
  )
  
  in_out[is.na(in_out)] = 0
  
  
  return(in_out)
  
}

