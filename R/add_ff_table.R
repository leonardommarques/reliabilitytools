
#'
#' Failure count
#'
#' add failure counts to plot
#'
#' @param ggaux: a plot reulting from \code{plot_predict_multi_surv_reg}.
#' @param multi_surv_reg: List containing at least the element 'ff_table'.
#' @return A ggplot.
#' @details Add geom_text containing the failure, suspension and total counts.
#' @export
#' @examples
#'

add_ff_table = function(ggaux, multi_surv_reg){
  
  # -- fault frequency table -- + 
  if(class(multi_surv_reg[['ff_table']]) == 'list'){
    aux_df = multi_surv_reg[['ff_table']] %>% 
      map(as.data.frame) %>% 
      bind_rows(.id = 'month')
    
  } else {
    aux_df = multi_surv_reg[['ff_table']]
  }
  
  # -- Month -- +
  # aux_df$month = names(multi_surv_reg[['ff_table']])
  aux_df$month = factor(aux_df$month
                        , levels = as.character(sort(unique(as.Date(aux_df$month))))
                        , ordered = TRUE)
  levels(aux_df$month) = format(as.Date(levels(aux_df$month)), format = "%b%y")
  
  
  # -- offset to separate each label -- +
  aux_df$offset = rep(c(0,4), ceiling(nrow(aux_df)/2)) %>% 
    head(nrow(aux_df))
  
  # -- Add geom_tex object -- +
  ggaux + 
    geom_text(data = aux_df
              ,aes(x = month
                   , y = -Inf
                   , label = failures
                   , vjust = -5 -offset
                   # , col = 'red'
              )
              , size = 3
              ,inherit.aes = FALSE
              , col = 'red'
    )+
    geom_text(data = aux_df
              ,aes(x = month
                   , y = -Inf
                   , label = suspensions
                   , vjust = -4 -offset
                   # , col = 'darkgreen'
              )
              , size = 3
              ,inherit.aes = FALSE
              , col = 'darkgreen'
    )+
    geom_text(data = aux_df
              ,aes(x = month
                   , y = -Inf
                   , label = total
                   , vjust = -3 -offset
                   # , col = 'darkblue'
              )
              , size = 3
              ,inherit.aes = FALSE
              , col = 'darkblue'
    ) + 
    # theme(legend.position = 'top') + 
    geom_point(data = data.frame(month = -Inf, variable = c('failures', 'suspensions', 'total'))
               ,aes(x = month
                    , y = -Inf
                    # , fill = variable
                    , show_guide = TRUE
                    , size = variable
               )
               # , size = 0
               ,inherit.aes = FALSE
               # , col = 'darkblue'
               , guides = 'none'
    ) + 
    
    scale_size_manual(values = c(c(0,0,0))) +
    guides(size = guide_legend(override.aes=list(size = c(4,4,4)
                                                 , col = c('red', 'darkgreen', 'darkblue')
    )
    , title = ''))
  
  
}
