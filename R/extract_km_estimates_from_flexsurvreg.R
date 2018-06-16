
###############################+
# Extracts the Kaplan-Meyer
###############################+

#'
#' Kaplan-Meyer survival estimates
#'
#' Extracts the Kaplan-Meyer survival estimates from a 'flexsurvreg' object
#'
#' @param m_flexsurvreg: an object or list of objects of class \code{flexsurvreg}. If m_flexsurvreg is a list the function will extract the data of only the first element.
#' @param ic: If \cpde{TRUE} returns the confidence interval
#' @return: A \code{data.framme} containing the Kaplan-Meyer survival estimates.
#' @details
#' @export
#' @examples
#'

extract_km_estimates_from_flexsurvreg = function(m_flexsurvreg, ic = FALSE){
  
  #--------------------------+
  # Extracts the Kaplan-Meyer survival estimates from a 'flexsurvreg' object
  # Returns a data framme containing the Kaplan-Meyer survival estimates
  #--------------------------+
  # m_flexsurvreg: an object or list of objects of class \code{flexsurvreg}. If m_flexsurvreg is a list the function will extract the data of only the first element.
  #--------------------------+
  
  if(class(m_flexsurvreg) == 'list')
    m_flexsurvreg = m_flexsurvreg[[1]]
  if(class(m_flexsurvreg) == 'flexsurvreg_NULL'){
    return()
  } else if(class(m_flexsurvreg) == 'flexsurvreg_faultless'){
    
    survfit.aux = data.frame(time=0
                             , surv = 1
                             , LCI = 1
                             , UCI = 1)
    return(survfit.aux)
  }
  
  # -- KM survival estimates
  aux.surv = m_flexsurvreg$data$m[[1]]
  survfit.aux = survfit(aux.surv ~ 1)
  # survfit.aux = m_flexsurvreg$data$m
  
  survfit.aux = summary(survfit.aux)
  survfit.aux = data.frame(time = survfit.aux[['time']]
                           , surv = survfit.aux[['surv']]
                           , LCI = survfit.aux[['lower']]
                           , UCI = survfit.aux[['upper']]
  )
  
  survfit.aux = rbind(data.frame(time=0
                                 , surv = 1
                                 , LCI = 1
                                 , UCI = 1
  ), survfit.aux)
  
  return(survfit.aux)
  
}

