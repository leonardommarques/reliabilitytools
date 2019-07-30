#' Bulk life time preparatio.
#'
#' Prepare life times for each individual in the dataset
#'
#' @param data: A \code{data.frame} to be prepared
#' @param indiv_col: column containing the identification of the individual.
#' @param obs_time_col: Observed time of the event
#' @param status_col: Event (O suspension, 1 "death")
#' @param limit: The maximum number of events per idividual to be considered.
#' @param skip: The ammount of events to skip. 
#' @return A \code{data.frame} conatining the prepared data.
#' @details Prepared the life times creating a column of \code{elapsed} times between events.
#' In cases that only the first n event should be considered, \code{limit = n} should be used.
#' If the first n events should not be considered, \code{skip = n} should be used.
#' @export
#' @examples
#' 
#' aux_df = data.frame(indiv = 1
#'                     , status = c(1,1,1,1,0)
#'                     , time = c(1,4,5,7,12))
#' 
#' # Considering only the 2 first failures
#' prepare_life_times(aux_df
#'                    , indiv_col = 'indiv'
#'                    , status_col = 'status'
#'                    , obs_time_col = 'time'
#'                    , limit = 2
#' )
#' 
#' # situation that once the component fails, it is replaced by a remanufactured one and we are studying the reliability of the remanufactured component.
#' prepare_life_times(aux_df
#'                    , indiv_col = 'indiv'
#'                    , status_col = 'status'
#'                    , obs_time_col = 'time'
#'                    , limit = 2
#'                    , skip = 1
#' )

prepare_life_times = function(data
                              ,indiv_col ='indiv'
                              ,status_col = 'status'
                              ,obs_time_col='obs_time'
                              ,limit = Inf
                              ,skip = 0
                              
){
  
  # argument check
  stopifnot(limit >=0)
  stopifnot(skip >=0)
  
  # Match the names of the columns to the function call
  names(data)[names(data) == indiv_col] = 'indiv'
  names(data)[names(data) == status_col] = 'status'
  names(data)[names(data) == obs_time_col] = 'obs_time'
  
  data = data.table(data)
  
  
  # ------------------------------------------------- #
  # Get last suspension
  #  - Split the data.frame into "events" and "suspensions"
  #  - order the suspenstions by ID and observed time and delete repeated IDs
  # ------------------------------------------------- #
  setDT(data)
  setorder(data,indiv,-obs_time)
  data_events = data[, .SD[status==1]]
  indiv_failures = unique(data_events$indiv) # individuals who have failed
  data_suspentions = data[, .SD[status!=1]]
  data_suspentions = unique(data_suspentions,by = c('indiv'))
  
  data = rbind(data_events,data_suspentions)
  setorder(data,indiv, -status,obs_time)
  
  # ------------------------------------------------- #
  # Creates the start, stop and time columns
  # ------------------------------------------------- #
  
  data[['aux_displaced_indiv_aux']] = c(data[['indiv']][1]
                                , head(data[['indiv']], -1))
  
  data[['aux_displaced_time_aux']] = c(0
                               , head(data[['obs_time']], -1))
  
  data[['aux_is_first_time_aux']] = as.integer(data[['aux_displaced_indiv_aux']] == data[['indiv']])
  # data[, aux_is_first_time_aux := as.integer(aux_displaced_indiv_aux == indiv)]
  
  
  data[,elapsed := obs_time - (aux_displaced_time_aux * aux_is_first_time_aux)]
  
  data[,c('aux_displaced_indiv_aux'
          , 'aux_displaced_time_aux'
          , 'aux_is_first_time_aux'
          )] = NULL
  
  # remove no npositive elapsed times
  data = data[elapsed > 0]
  
  # ------------------------------------------------- #
  # limit ans skip the amount of events
  # ------------------------------------------------- #
  
  if(limit > 0 | skip > 0){
    
    # failure_number = data[,c('indiv', 'status')]
    # failure_number = failure_number[,.(n_fail = sum(status)), , by=indiv]
    # failure_number = failure_number[n_fail > 0]
    # 
    # df_failures = data[indiv %in% failure_number[['indiv']]]
    # df_suspensions = data[!(indiv %in% failure_number[['indiv']])]
    
    # df_failures = split(df_failures, by = 'indiv')
    # df_failures = df_failures %>%
    #   llply(function(aux_df){
    #     
    #     # skip first n
    #     # if(skip > 0) 
    #     aux_df = tail(aux_df, -skip)
    #     
    #     # limit number of failures
    #     # if(limit > 0)
    #     aux_df = head(aux_df, limit)
    #     })
    
    data[['aux_displaced_indiv_aux']] = c(data[['indiv']][1]
                                          , head(data[['indiv']], -1))
    
    data[['aux_displaced_indiv_aux']] = data[['aux_displaced_indiv_aux']] == data[['indiv']]
    data[['ith_failure']] = 0
    data[['ith_failure']][[1]] = 1
    
    i = 1
    while(i < nrow(data)){
      i = i+1
      # ith_failure = 0
      data[['ith_failure']][i] = data[['ith_failure']][[i-1]]*data[['aux_displaced_indiv_aux']][i] + 1

    }
    
    data = data[data$ith_failure > skip]
    data = data[data$ith_failure <= skip+limit]
    data[,c('aux_displaced_indiv_aux', 'ith_failure')] = NULL

  }
  
  # Original names
  names(data)[names(data) == 'indiv'] = indiv_col
  names(data)[names(data) == 'status'] = status_col
  names(data)[names(data) == 'obs_time'] = obs_time_col
  
  
  return(data)
  
}

