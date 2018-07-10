# this function returns the complement of the punif. It is needed because 
# there is no distribution that return 0 when LOWER = FALSE
punif_compl = function(...){
  1-punif(...)
}

qunif_compl = function(...){
  1-qunif(...)
}


