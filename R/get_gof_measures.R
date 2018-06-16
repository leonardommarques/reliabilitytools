
#'
#' Parametrical and non parametrical fit compare.
#'
#' Compare flexsurv and survfit
#'
#' @param KM_fit: a \code{survfit} object
#' @param m_flexsurvreg: a \code{flexsurvreg} object
#' @return: A \code{data.table} with the comparison results.
#' @details # Returns a \code{data.frame} with the columns
#' fit_mse: The mean squared error of the linear model %$S_{km} ~ S_{model$%
#' mse: average of the squared difference between %$S_{km} - S_{model$%
#' rho_KM_flex: correlation between %$S_{km} $% and %$S_{model$%
#' loglik: log likelihood of the model
#' p_ks_test: p-value of the Kolmogorov-Smirnov test \code{ks.test()}
#' @export
#' @examples
#'

get_gof_measures = function(KM_fit, m_flexsurvreg){
  # ------------------------------------ +
  # Compares flexsurv and survfit
  # ------------------------------------ +
  
  #
  # ------------------------------------+
  # Returns a \code{data.frame} with the columns
  # fit_mse: The mean squared error of the linear model %$S_{km} ~ S_{model$%
  # mse: average of the squared difference between %$S_{km} - S_{model$%
  # rho_KM_flex: correlation between %$S_{km} $% and %$S_{model$%
  # loglik: log likelihood of the model
  # p_ks_test: p-value of the Kolmogorov-Smirnov test \code{ks.test()}
  # ------------------------------------+
  
  
  m_flexsurvreg$data$Y %>% data.frame() %>% filter(status == 1) %>% arrange(time)
  m_flexsurvreg
  KM_fit$time[KM_fit$n.event > 0]
  KM_fit[KM_fit$n.event > 0]
  
  # --- data.frame for comparation --- +
  # kaplan meier
  KM_fit_summ = summary(KM_fit)
  compare_df = data.frame(time = KM_fit_summ[['time']]
                          , surv_km = KM_fit_summ[['surv']]
                          # , events = KM_fit_summ[['n.event']]
  )
  # add flexsurvreg estimates
  compare_df['surv_flexsurvreg'] =  summary(m_flexsurvreg, t = KM_fit_summ[['time']])[[1]][['est']]
  
  
  correlation = with(compare_df, cor(surv_km ,surv_flexsurvreg))
  mse = with(compare_df, mean((surv_km-surv_flexsurvreg)^2))
  loglik = m_flexsurvreg[['loglik']]
  
  # -- linear model of the residuals -- #
  fit = lm(surv_km ~ surv_flexsurvreg, compare_df)
  fit_mse = fit$residuals^2 %>% mean()
  
  # -- fit test -- #
  p_ks_test = with(compare_df, ks.test(surv_flexsurvreg,surv_km))$p.value
  
  result_df = tibble(fit_mse = fit_mse,
                     p_ks_test = p_ks_test,
                     mse = mse,
                     rho_KM_flex = correlation,
                     loglik = loglik,
                     fit = list(fit)
  )
  
  return(result_df)
  
}
