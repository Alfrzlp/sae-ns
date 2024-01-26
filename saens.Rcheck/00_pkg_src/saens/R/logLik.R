#' Extract Log-Likelihood
#'
#' @param object EBLUP model
#' @param ... further arguments passed to or from other methods.
#'
#' @return Log-Likehood value
#'
#' @examples
#' library(saens)
#'
#' model1 <- eblupfh_cluster(y ~ x1 + x2 + x3, data = mys, vardir = "var", cluster = "clust")
#' logLik(model1)
#'
#' @export
logLik.eblupres <- function(object, ...) {
  return(object$fit$goodness[1])
}
