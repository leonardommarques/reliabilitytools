
#' Safe version of flexsurv::flexsurvreg()
#'
#' Safe version of flexsurv::flexsurvreg()
#'
#' @param ...
#' @return 
#' @export
#' @examples
#'
safe_flexsurvreg = safely(flexsurvreg,quiet=TRUE)

