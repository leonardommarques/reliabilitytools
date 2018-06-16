#' Survival Spline fit
#'
#' Finds best spline suvreg fit
#'
#' @param knots: Number of knots.
#' @param scale_list: A vector containing a list of scale arguments for \code{flexsurvspline}.
#' @param status_col: Name of the column containing the status.
#' @param surv_data: a \code{data.frame}
#' @param surv_formula: a \code{formula}
#' @param time_col: Name of the column containing the time.
#' @param ...: further arguments for \code{flexsurvspline}.
#' @return The models are returned in the \code{models} slot of the result, ordered by best fit. The slot \code{erros} contains the fits that had erros. \code{Adjust_table} contains the goodness of fit of the models. \code{ff_table} contains the faulire frequency table. \code{KM_fit} has the Kaplan-Meier fit.
#' @details
#' @export
#' @examples
#'

multi_surv_spline = function(surv_data
                               ,time_col='time'
                               ,status_col='status'
                               ,surv_formula = '~ 1'
                               ,scale_list = ''
                               ,knots = 3
                               ,...
){
  ###############################+
  # returns a list containing the best splinesurvreg model
  
  # scale_list: a vector containing a list of scale arguments for \code{flexsurvspline}.
  ###############################+
  
  # List of scales
  if(scale_list == ''){
    scale_list = c("hazard"
                   ,"odds"
                   ,"normal")
  }
  
  # Changes the names of the columns to match 'time' and 'status'
  names(surv_data)[names(surv_data) == time_col] = 'time'
  names(surv_data)[names(surv_data) == status_col] = 'status'
  
  # Creates suv object and formula
  aux_surv = with(surv_data,Surv(time, status))
  surv_formula = paste('aux_surv',surv_formula)
  surv_formula = as.formula(surv_formula)
  
  # Failure frequence table
  ff_table = table(surv_data$status)
  ff_table = matrix(c(ff_table,sum(ff_table)),ncol=3)
  colnames(ff_table) = c('suspensions','failures','total')
  
  
  # Model/fit
  models = lapply(scale_list
                  ,function(x){
                    safe_flexsurvspline(surv_formula
                                        , data=surv_data
                                        , k=knots
                                        , scale=x
                                        , ...)
                    
                  }
  )
  names(models) = scale_list
  
  
  # Devides into models that had and didn't have errors
  models_erros  = models %>% map('error')
  models = models %>% map('result')
  
  # Removes NULL
  index = unlist(map(models,is.null))
  models = models[!index]
  index = unlist(map(models_erros,is.null))
  models_erros = models_erros[!index]
  
  # orders adjusts by AIC (Only if there were adjusts that without errors)
  if(length(models) >0){
    
    # loglikelyhood
    loglik = unlist(map(models,"loglik"))
    
    # Pearson's correlation between KM and flexsurv suvival estimates
    rhoK_M_flex = map(models,compare_KM_flexsurvreg)
    rho_KM_flex_list = map(rho_KM_flex, function(xx) attr(xx,'est.compare'))
    rho_KM_flex = unlist(map(models,compare_KM_flexsurvreg))
    
    
    # data.frame with all measures
    aux_AIC = data.frame(rho_KM_flex,loglik)
    aux_AIC$distr = as.character(rownames(aux_AIC))
    
    
    index = order(aux_AIC[,'rho_KM_flex'],decreasing = TRUE)
    aux_AIC = aux_AIC[index,]
    rownames(aux_AIC) = NULL
    models = models[aux_AIC$distr]
    
  } else {
    models = 'No models were adjusted without errors'
    print(models)
    return(NULL)
  }
  
  # if(length(models) == 1)
  #   models = models[[1]]
  
  return(list(models = models
              ,erros = models_erros
              ,Adjust_table = aux_AIC
              ,ff_table = ff_table
              ,KM_flex_list = rho_KM_flex_list)
  )
  
}
