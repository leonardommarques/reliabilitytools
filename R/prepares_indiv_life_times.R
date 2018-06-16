
#' Life time prepare
#'
#' Preprares life times of one individual
#'
#' @param data: A \code{data.frame}
#' @param indiv_col: column containing the identification of the individual.
#' @param obs_time_col: Observed time of the event
#' @param status_col: Event (O suspension, 1 "death")
#' @return A \code{data.frame} appropriately prepared for reliability analysis.
#' @details The codes gets the registered times and status and transforms them into life data.
#' Especifications:
#'   -If the longest time is a failure there is no need to take suspenstions into account.
#'   -Only the longest observed suspension has to be considered.
#'   -Lifetimes equal to Zero will no be considered:
#'     +If an individual has failed from two causes at the same time there will be tied events.
#'       This will lead to events with liefetime equal to Zero, which can not be considered for the survival computations
#'     +Failures occouring at time Zero can not be considered for the survival calculations nor the failure cause table.
#' @export
#' @examples
#'

prepare_indiv_life_times = function(data
                                       ,indiv_col='indiv'
                                       ,obs_time_col='obs_time'
                                       ,status_col = 'status'){
  
  
  
  # data: A data frame with the following columns:
  # indiv: code of the individual
  # obs_time: Observed time of the event
  # status: Event (O suspension, 1 "death")
  # browser()
  
  # Match column names to function call
  names(data)[names(data) == indiv_col] = 'indiv'
  names(data)[names(data) == obs_time_col] = 'obs_time'
  names(data)[names(data) == status_col] = 'status'
  
  
  index = with(data,order(indiv, obs_time) )
  data = data[index,]
  
  # If the longest time is a failure there is no need to take suspenstions into account
  if(data[which.max(data$obs_time),'status'] == 1){
    index = data$status != 0
    data = data[index,]
  } else {
    index = data$status != 0
    data.failures = data[index,]
    
    # Only the longest observed suspension has to be considered.
    data.suspensions = data[!index,]
    data.suspensions = data.suspensions[which.max(data.suspensions$obs_time),]
    data = rbind(data.failures,data.suspensions)
  }
  
  
  index = order(data$obs_time)
  data = data[index,]
  # Creates the start, stop and time columns
  data$start = c(0, data$obs_time[-length(data$obs_time)])
  data$stop = data$obs_time
  data$time = data$stop - data$start
  
  # -Lifetimes equal to Zero will no be considered
  index = data$time == 0
  data = data[!index,]
  
  
  # Original Names
  names(data)[names(data) == 'indiv'] = indiv_col
  names(data)[names(data) == 'status'] = status_col
  names(data)[names(data) == 'obs_time'] = obs_time_col
  
  
  return(data)
}

