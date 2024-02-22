#' Extract Model Coefficients.
#'
#' @param object EBLUP model.
#' @param ... further arguments passed to or from other methods.
#'
#' @return model coefficients
#'
#' @examples
#' m1 <- eblupfh_cluster(y ~ x1 + x2 + x3, data = mys, vardir = "var", cluster = "clust")
#' coef(m1)
#'
#' @export
coef.eblupres <- function(object, ...) {
  est_beta <- object$fit$estcoef$beta
  names(est_beta) <- rownames(object$fit$estcoef)
  return(est_beta)
}
