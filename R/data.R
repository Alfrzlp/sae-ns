#' @title mys: mean years of schooling people with disabilities in Papua Island, Indonesia.
#' @description A dataset containing the mean years of schooling people with disabilities in Papua Island, Indonesia in 2021.
#' @format
#' A data frame with 42 rows and 7 variables with 10 domains are non-sampled areas.
#'
#' \describe{
#'   \item{area}{regency municipality}
#'   \item{y}{mean years of schooling people with disabilities}
#'   \item{var}{variance sampling from the direct estimator for each area}
#'   \item{rse}{relative standard error (\%)}
#'   \item{x1}{Number of Elementary Schools}
#'   \item{x2}{Number of Junior High Schools}
#'   \item{x3}{Number of Senior High Schools}
#'   \item{clust}{Cluster}
#' }
#' @source \url{https://www.bps.go.id}
"mys"

#' @title milk: Data on fresh milk expenditure.
#' @description Data on fresh milk expenditure, used by Arora and Lahiri (1997) and by You and Chapman (2006).
#' @format
#' A data frame with 43 observations on the following 6 variables.
#'
#' \describe{
#'   \item{SmallArea}{areas of inferential interest.}
#'   \item{ni}{sample sizes of small areas.}
#'   \item{yi}{average expenditure on fresh milk for the year 1989 (direct estimates for the small areas).}
#'   \item{SD}{estimated standard deviations of yi.}
#'   \item{var}{variance sampling from the direct estimator (yi) for each area}
#'   \item{CV}{estimated coefficients of variation of yi.}
#'   \item{MajorArea}{major areas created by You and Chapman (2006). These areas have similar direct estimates and produce a large CV reduction when using a FH model.}
#' }
#'
#' @references
#' \enumerate{
#'   \item Arora, V. and Lahiri, P. (1997). On the superiority of the Bayesian method over the BLUP in small area estimation problems. Statistica Sinica 7, 1053-1063.
#'   \item You, Y. and Chapman, B. (2006). Small area estimation using area level models and estimated sampling variances. Survey Methodology 32, 97-103.
#' }
"milk"
