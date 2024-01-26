#' Akaike's An Information Criterion
#'
#' @description Generic function calculating Akaike's "An Information Criterion" for EBLUP model
#'
#' @param object EBLUP model
#' @param ... further arguments passed to or from other methods.
#'
#' @return AIC value
#'
#' @examples
#' m1 <- eblupfh_cluster(y ~ x1 + x2 + x3, data = mys, vardir = "var", cluster = "clust")
#' AIC(m1)
#'
#' @export
AIC.eblupres <- function(object, ...) {
  return(object$fit$goodness[2])
}

#' @rdname AIC.eblupres
#' @importFrom stats BIC
#' @export
BIC.eblupres <- function(object, ...) {
  return(object$fit$goodness[3])
}
