
#' Best suvreg adjust
#'
#' Finds best suvreg adjust 
#'
#' @param dist_list: A vector containing a list of distributions to fit survival models and pick the best.
#' @param status_col: The ame of the column containing the status
#' @param surv_data: a \code{data.frame}
#' @param surv_formula: A \code{formula} for the regression.
#' @param time_col: name of the column containing the time
#' @param ...: further arguments for flexsurvreg()
#' @return A \code{list} containing the best survreg fits
#' @details All distributions in the \code{dist_list} are used as link functions for the survival regresion model. The models are returned in the \code{models} slot of the result, ordered by best fit. The slot \code{erros} contains the fits that had erros. \code{Adjust_table} contains the goodness of fit of the models. \code{ff_table} contains the faulire frequency table. \code{KM_fit} has the Kaplan-Meier fit.
#' @export
#' @examples
#' 
#' da = system.file("extdata", "example.csv", package = "reliabilitytools")
#' da = read.csv2(da)
#' 
#' da_status = da %>%#'   
#'   mutate(status = as.integer(failure_cause_group == 560)) # cause of failure
#'   
#'   
#' # -- prepare life data
#' da_prepared = prepare_life_times(da_status
#'                                  , indiv_col = 'id'
#'                                  , status_col = 'status'
#'                                  , obs_time_col = 'cycles')
#' # -- suvival models
#' surv_model = multi_surv_reg(da_prepared
#'                             , time_col = 'elapsed'
#'                             , status_col = 'status')
#' 
#' # -- plot
#' plot_life_curve(surv_model$models,
#'                 title_aux = '\nFailure cause: 560')
#' 

multi_surv_reg = function(surv_data
                            ,time_col='time'
                            ,status_col='status'
                            ,surv_formula = '~ 1'
                            ,dist_list = ''
                            ,...
){
  
  
  if(dist_list == ''){
    dist_list = c("gengamma"
                  # ,"gengamma.orig" # way too many errors at the optimization part. Mostly due to initial values
                  # ,"genf" # the AIC was negative, I was not secure of the result.
                  # ,"genf.orig"
                  ,"weibull"
                  ,"gamma"
                  # ,"exp"
                  ,"llogis"
                  # ,"lnorm"
                  # ,"gompertz"
                  ,"exponential"
                  ,"lognormal")
  }
  
  # -- creates survival data
  surv_data = data.frame(surv_data)
  
  # NO_failures = nrow(surv_data) == 0 | sum(surv_data[,status_col]) == 0
  NO_failures = sum(surv_data[,status_col]) == 0
  NO_DATA = nrow(surv_data) == 0
  
  # -- No Data -- #
  if(NO_DATA){
    
    # -- If there are no failures there is no need for fitting a survival curve.
    
    NULL_model = list(NULL_model = 'NULL_model')
    class(NULL_model) = "flexsurvreg_NULL"
    NULL_model$dlist = list('name' = 'NULL_model')
    NULL_model$data = list(m = surv_data) #list(m = with(surv_data,Surv(time, status)))
    
    suspensions = dim(surv_data)[1]
    ff_table = matrix(c(suspensions,0,suspensions),nrow = 1, dimnames = list('', c("suspensions", "failures", "total")))
    
    
    # -- result object
    return(list(models = list(NULL_model = NULL_model)
                ,erros = ''
                ,Adjust_table = NA
                ,ff_table = ff_table))
    
  }
  
  # -- No failure -- #
  if(NO_failures){
    
    faultless_model = list(faultless_model = 'faultless_model')
    class(faultless_model) = "flexsurvreg_faultless"
    
    # -- distribution information
    faultless_model$dlist = list('name' = 'unif_compl')
    res = matrix(c(0,0),ncol = 1)
    rownames(res) = c('min', 'max')
    colnames(res) = c('est')
    faultless_model$res = res
    
    # -- failure table
    suspensions = dim(surv_data)[1]
    ff_table = matrix(c(suspensions,0,suspensions),nrow = 1, dimnames = list('', c("suspensions", "failures", "total")))
    
    # -- KM fit
    # Changes the names of the columns to match 'time' and 'status'
    names(surv_data)[names(surv_data) == time_col] = 'time'
    names(surv_data)[names(surv_data) == status_col] = 'status'
    
    # Creates surv object and formula
    aux_surv = with(surv_data,Surv(time, status))
    surv_formula = paste('aux_surv',surv_formula)
    surv_formula = as.formula(surv_formula)
    
    # KM (Kaplan Meier) survival estimates
    KM_fit = survfit(surv_formula)  
    
    
    faultless_model$data = list(m = KM_fit)
    
    
    # -- result object
    result = list(models = list(faultless_model = faultless_model)
                ,erros = ''
                ,Adjust_table = NA
                ,ff_table = ff_table
                ,KM_fit = KM_fit)
    
    class(result) = 'multi_surv_reg'
    return(result)
    
  }
  
  # Changes the names of the columns to match 'time' and 'status'
  names(surv_data)[names(surv_data) == time_col] = 'time'
  names(surv_data)[names(surv_data) == status_col] = 'status'
  
  # -- ff_table -- #
  ff_table = table(factor(surv_data$status, levels = 0:1))
  ff_table = matrix(c(ff_table,length(surv_data$status)),ncol=3)
  colnames(ff_table) = c('suspensions','failures','total')
  
  # Creates surv object and formula
  aux_surv = with(surv_data,Surv(time, status))
  surv_formula = paste('aux_surv',surv_formula)
  surv_formula = as.formula(surv_formula)
  
  # KM (Kaplan Meier) survival estimates
  KM_fit = survfit(surv_formula)  
  
  
  
  # Fits a model for each distribution (conditional to numer of failures >0)
  if(ff_table[,'failures']>0){
    models = lapply(dist_list
                    ,function(x){
                      # browser()
                      safe_flexsurvreg(formula = surv_formula
                                       ,data=surv_data
                                       ,dist=x
                                       ,...)
                    }
    )
    
    names(models) = dist_list
  } else {
    # -- the model
    models = lapply(dist_list
                    ,function(x){
                      safe_flexsurvreg(surv_formula
                                       ,data=surv_data
                                       ,dist=x,...)
                    })
    
    # rename the distribution
    names(models) = 'no_failures' # dist_list
    # no_failures
    models = llply(models
                   ,function(xx){
                     aux = xx
                     if(class(xx$result) == 'flexsurvreg'){
                       aux$result$dlist$name = 'no_failures'
                     }
                     return(aux)
                   }
    )
  }
  
  
  # Splits into models that had and didn't have errors
  models_erros = models %>% map('error')
  models = models %>% map('result')
  
  # Removes NULL
  index = unlist(map(models,is.null))
  models = models[!index]
  index = unlist(map(models_erros,is.null))
  models_erros = models_erros[!index]
  
  # order adjusts by "K-M VS MODEL" correlation and AIC (Only if there were adjusts that without errors and failures in the data.base)
  if(length(models) > 0){
    
    # Goodness of fit
    if(ff_table[,'failures']>0){
      # a data frame with, mean squared errors, corelation and likelihood
      gof_df = models %>% 
        map(get_gof_measures,KM_fit = KM_fit) %>% 
        rbind_list()
      
      gof_df$distr = names(models)
      
      # -- reorder ----- +
      index = with(gof_df,
                   order(fit_mse,
                         -p_ks_test,
                         mse,
                         -rho_KM_flex,
                         -loglik)
      )
      gof_df = gof_df[index,]
      rownames(gof_df) = NULL
      models = models[gof_df$distr]
      # gof_df = data.frame(gof_df)
      
    } else {
      
      # gof_df = models %>%
      #   map(get_gof_measures,KM_fit = KM_fit) %>%
      #   rbind_list()
      # gof_df$distr = names(models)
      
      rho_KM_flex = rep(NA,each=length(models))
      mse = rep(NA,each=length(models))
      names(rho_KM_flex) = names(models)
      loglik =  rep(NA,each=length(models)) #models[[1]][['loglik']] 
      gof_df = data.frame(mse, rho_KM_flex,loglik)
      gof_df$distr = names(models)
      
    }
    
  } else {
    models = 'No model adjusted without errors'
    # print(models)
    class(models) = "flexsurvreg_NULL"
    models = list(flexsurvreg_NULL = models)
    rho_KM_flex = rep(NA,each=length(models))
    mse = rep(NA,each=length(models))
    names(rho_KM_flex) = names(models)
    loglik =  rep(NA,each=length(models))
    gof_df = data.frame(mse, rho_KM_flex,loglik)
    gof_df$distr = 'flexsurvreg_NULL'
    
  }
  
  
  
  
  result = list(models = models
                ,erros = models_erros
                ,Adjust_table = gof_df
                ,ff_table = ff_table %>% data.frame()
                ,KM_fit = KM_fit)
  
  class(result) = c('multi_surv_reg', 'list')
  
  return(result)
  
  
  
}

