###############################+
# print & plot method for compare_KM_flexsurvreg class
###############################+

print.compare_KM_flexsurvreg <- function(x, ...) { 
  attributes(x) <- NULL 
  print(x) 
}

plot.compare_KM_flexsurvreg <- function(x, ...) { 
  x = attr(x,'est.compare')
  plot(surv_flex~surv_KM, data = x,...)
  abline(a=0, b=1,col=2)
} 