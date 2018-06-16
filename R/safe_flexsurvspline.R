
###############################+
# Safe version of flexsurv::flexsurvspline() ####
###############################+


#' Safe version of flexsurv::flexsurvspline()
#'
#' Safe version of flexsurv::flexsurvspline()
#'
#' @param ...
#' @return 
#' @details
#' @export
#' @examples
#'
safe_flexsurvspline = safely(flexsurvspline,quiet=TRUE)

