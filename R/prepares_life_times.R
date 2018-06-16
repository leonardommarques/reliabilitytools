#' Bulk life time preparatio.
#'
#' Prepare life times for each individual in the dataset
#'
#' @param data: A \code{data.frame} to be prepared
#' @param indiv_col: column containing the identification of the individual.
#' @param obs_time_col: Observed time of the event
#' @param status_col: Event (O suspension, 1 "death")
#' @return A \code{data.frame} conatining the prepared data.
#' @details The codes calls prepare_indiv_life_times for each individual. 
#' @export
#' @examples
#' 
#' da = system.file("extdata", "example.csv", package = "reliabilitytools")
#' da = read.csv2(da)
#' 
#' da %>%
#'   mutate(status = as.integer(failure_cause_group == 560)) %>%
#'   prepare_life_times(indiv_col = 'id'
#'                      , status_col = 'status'
#'                      , obs_time_col = 'cycles')
#'
#'
prepare_life_times = function(data
                                 ,indiv_col ='indiv'
                                 ,status_col = 'status'
                                 ,obs_time_col='obs_time'
){
  # Match the names of the columns to the function call
  names(data)[names(data) == indiv_col] = 'indiv'
  names(data)[names(data) == status_col] = 'status'
  names(data)[names(data) == obs_time_col] = 'obs_time'
  
  data = data.table(data)
  
  ################################+
  # Split the data.frame into events and suspenssions, orders the suspenstions by ID and observed time and deletes repeated IDs
  setDT(data)
  setorder(data,indiv,-obs_time)
  data_events = data[, .SD[status==1]]
  indiv_failures = unique(data_events$indiv) # individuals who have failed
  data_suspentions = data[, .SD[status!=1]]
  data_suspentions = unique(data_suspentions,by = c('indiv'))
  
  ################################+
  
  data = rbind(data_events,data_suspentions)
  
  # Creates the start, stop and time columns
  index = data$indiv %in% indiv_failures
  data$indiv_failures = 0
  data$indiv_failures[index] = 1
  
  # Start-stop for suspetions
  data_suspentions = data[indiv_failures==0,]
  data_suspentions$start = 0
  data_suspentions$stop = data_suspentions$obs_time
  data_suspentions$elapsed = data_suspentions$stop
  
  # Start-stop for failures
  data_eventos = data[indiv_failures==1]
  setorder(data_eventos,indiv,-status,obs_time)
  
  failure_data = lapply(indiv_failures,function(xx){
    aux_data = data_eventos[indiv == xx,] # data.table way to filter lines
    # order by obs_time
    aux_data$start = c(0, aux_data$obs_time[-length(aux_data$obs_time)])
    aux_data$stop = aux_data$obs_time
    aux_data$elapsed = aux_data$stop - aux_data$start
    aux_data
  })
  
  failure_data = do.call(rbind,failure_data)
  data = rbind(failure_data,data_suspentions)
  
  data = data[elapsed > 0,] # remove times smaller or equal zero
  data[,indiv_failures:=NULL] # remove no longer usefull column
  
  
  # Original names
  names(data)[names(data) == 'indiv'] = indiv_col
  names(data)[names(data) == 'status'] = status_col
  names(data)[names(data) == 'obs_time'] = obs_time_col
  
  
  return(data)
  
}

