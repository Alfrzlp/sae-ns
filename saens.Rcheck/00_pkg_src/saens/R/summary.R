#' Summarizing EBLUP Model Fits
#'
#' @description `summary` method for class "eblupres"
#' @param object EBLUP model
#' @param ... further arguments passed to or from other methods.
#'
#' @return The function returns a data frame that contains the following columns: \cr
#'    * \code{y} variable response \cr
#'    * \code{eblup} estimated results for each area \cr
#'    * \code{random_effect} random effect for each area \cr
#'    * \code{vardir} variance sampling from the direct estimator for each area \cr
#'    * \code{mse} Mean Square Error \cr
#'    * \code{cluster} cluster information for each area \cr
#'    * \code{rse} Relative Standart Error (%) \cr
#'
#' @examples
#' library(saens)
#'
#' model1 <- eblupfh_cluster(y ~ x1 + x2 + x3, data = mys, vardir = "var", cluster = "clust")
#' summary(model1)
#'
#' @export
summary.eblupres <- function(object, ...) {
  stats::printCoefmat(object$fit$estcoef, signif.stars = TRUE, ...)
}
