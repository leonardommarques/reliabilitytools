#'
#' Step axis grid breaks
#'
#' set grid breaks by step
#'
#' @param step: the step.
#' @param x: limits
#' @return Return break vector with the given 'step'
#' @details In scale_(x|y)_continuous, one can only explicitly set the grid breaks. With this function one can specify the breaks with a step.
#' @export
#' @examples
#' ggplot(cars, aes(dist, speed)) + 
#'   geom_point()+
#'   scale_x_continuous(breaks 
#'

my_break_fun = function(x,step = 50000){
  # -------------------------- #
  # Return break vector with the given 'step'
  # In scale_(x|y)_continuous, one can only explicitly set the grid breaks.
  # with this function one can specify the breaks with a step
  # -------------------------- #
  # x: limits
  # step: step
  # -------------------------- #
  # example:  ggplot(cars, aes(dist, speed)) + geom_point()+
  #           scale_x_continuous(breaks = function(x) aux_fun(x,step = 10.3))
  
  xx = pretty(x,10)
  xx = c(min(xx), max(xx))
  
  return(seq(xx[1],xx[2], by = step))
  
}
