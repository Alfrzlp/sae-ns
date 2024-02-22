#' EBLUPs based on a Fay-Herriot Model.
#'
#' @description This function gives the 'EBLUP' (or EB predictor under normality) based on a 'Fay-Herriot' model.
#'
#' @references
#' \enumerate{
#'  \item Rao, J. N., & Molina, I. (2015). Small area estimation. John Wiley & Sons.
#'  \item Anisa, R., Kurnia, A., & Indahwati, I. (2013). Cluster information of non-sampled area in small area estimation. E-Prosiding Internasional| Departemen Statistika FMIPA Universitas Padjadjaran, 1(1), 69-76.
#'}
#'
#' @param formula an object of class formula that contains a description of the model to be fitted. The variables included in the formula must be contained in the data.
#' @param data a data frame or a data frame extension (e.g. a tibble).
#' @param vardir vector or column names from data that contain variance sampling from the direct estimator for each area.
#' @param method Fitting method can be chosen between 'ML' and 'REML'.
#' @param maxiter maximum number of iterations allowed in the Fisher-scoring algorithm. Default is 100 iterations.
#' @param precision convergence tolerance limit for the Fisher-scoring algorithm. Default value is 0.0001.
#' @param scale scaling auxiliary variable or not, default value is FALSE.
#' @param print_result print coefficient or not, default value is TRUE.
#'
#' @returns The function returns a list with the following objects (\code{df_res} and \code{fit}):
#' \code{df_res} a data frame that contains the following columns: \cr
#'    * \code{y} variable response \cr
#'    * \code{eblup} estimated results for each area \cr
#'    * \code{random_effect} random effect for each area \cr
#'    * \code{vardir} variance sampling from the direct estimator for each area \cr
#'    * \code{mse} Mean Square Error \cr
#'    * \code{rse} Relative Standart Error (%) \cr
#'
#' \code{fit} a list containing the following objects: \cr
#'    * \code{estcoef} a data frame with the estimated model coefficients in the first column (beta),
#'    their asymptotic standard errors in the second column (std.error),
#'    the t-statistics in the third column (tvalue) and the p-values of the significance of each coefficient
#'    in last column (pvalue) \cr
#'    * \code{model_formula} model formula applied \cr
#'    * \code{method} type of fitting method applied (`ML` or `REML`) \cr
#'    * \code{random_effect_var} estimated random effect variance \cr
#'    * \code{convergence} logical value that indicates the Fisher-scoring algorithm has converged or not \cr
#'    * \code{n_iter} number of iterations performed by the Fisher-scoring algorithm. \cr
#'    * \code{goodness} vector containing several goodness-of-fit measures: loglikehood, AIC, and BIC \cr
#'
#'
#' @details
#' The model has a form that is response ~ auxiliary variables.
#' where numeric type response variables can contain NA.
#' When the response variable contains NA it will be estimated with cluster information.
#'
#' @export
#' @examples
#' library(saens)
#'
#' m1 <- eblupfh(y ~ x1 + x2 + x3, data = na.omit(mys), vardir = "var")
#' m1 <- eblupfh(y ~ x1 + x2 + x3, data = na.omit(mys), vardir = ~var)
#'
#' @md

eblupfh <- function(formula, data, vardir, method = "REML",
                    maxiter = 100, precision = 1e-04, scale = FALSE,
                    print_result = TRUE) {
  y <- stats::model.frame(formula, data, na.action = NULL)[[1]]
  # data hasil
  df_res <- data.frame(
    y = y,
    eblup = NA,
    random_effect = NA,
    vardir = NA,
    g1 = NA, g2 = NA, g3 = NA,
    mse = NA
  )


  if (any(is.na(y))) {
    cli::cli_abort("variable y contains NA values, please use eblupfh cluster function")
  }
  datas <- data

  vardir <- .get_variable(datas, vardir)
  formuladata <- stats::model.frame(formula, datas, na.action = NULL)
  X <- stats::model.matrix(formula, datas)
  y <- as.matrix(formuladata[1])

  if (scale) {
    X <- scale(X)
    # my_scale <- attr(X, "scaled:scale")
    # my_center <- attr(X, "scaled:center")
  }

  # Cek pilihan metode
  if (!toupper(method) %in% c("ML", "REML")) {
    cli::cli_abort('"method" must be ML or REML, not {method}')
  }
  # cek vardir mengandung NA atau tidak
  if (any(is.na(vardir))) {
    cli::cli_abort("Argument vardir contains NA values.")
  }
  # cek Auxiliary variabels mengandung NA atau tidak
  if (any(is.na(X))) {
    cli::cli_abort("Auxiliary variabels contains NA values.")
  }

  # inisialisasi untuk output
  result <- list(
    df_res = NA,
    fit = list(
      estcoef = NA,
      model_formula = formula,
      method = method,
      random_effect_var = NA,
      convergence = TRUE,
      n_iter = 0,
      goodness = NA
    )
  )


  # Inisialisasi variabel
  m <- nrow(X)
  p <- ncol(X)
  Xt <- t(X)
  k <- 0
  diff <- 1 + precision
  sigma2_u <- stats::median(vardir)
  R <- diag(vardir, m)
  Z <- diag(1, m)

  # Fisher scoring algorithm
  if (method == "ML") {
    while ((diff > precision) & (k < maxiter)) {
      # inisialisasi varians pengaruh acak (sigma2 u)
      G <- diag(sigma2_u[k + 1], m)
      # varians y
      V <- Z %*% G %*% t(Z) + R
      print(Z %*% G %*% t(Z))
      print(V)

      Vi <- solve(V)
      XtVi <- Xt %*% Vi

      # B = (X' V-1 X)-1 X' V-1 y
      # (Rao & Molina, 2015) equation 5.2.5
      Q <- solve(XtVi %*% X)
      BETA <- Q %*% XtVi %*% y

      # Partial derivative of log-likelihood with respect to sigma2_u
      # (Rao & Molina, 2015) after equation 5.2.16
      yXBeta <- y - X %*% BETA
      ViZtZ <- Vi %*% Z %*% t(Z)
      s <- -0.5 * sum(diag(ViZtZ)) - 0.5 * t(yXBeta) %*% (-ViZtZ %*% Vi) %*% (yXBeta)

      # Matrix of expected second derivaties of -log likehood
      # with respect to sigma2 u (Rao & Molina, 2015) equation 5.2.17
      Isigma2_u <- sum(diag(ViZtZ %*% ViZtZ)) / 2

      k <- k + 1
      # (Rao & Molina, 2015) equation 5.2.18
      sigma2_u[k + 1] <- sigma2_u[k] + s / Isigma2_u
      diff <- abs((sigma2_u[k + 1] - sigma2_u[k]) / sigma2_u[k])
    }
  } else if (method == "REML") {
    while ((diff > precision) & (k < maxiter)) {
      # inisialisasi varians pengaruh acak (sigma2 u)
      G <- diag(sigma2_u[k + 1], m)
      # varians y
      V <- Z %*% G %*% t(Z) + R

      Vi <- solve(V)
      XtVi <- Xt %*% Vi

      Q <- solve(XtVi %*% X)
      # (Rao & Molina, 2015) equation 5.2.21
      P <- Vi - Vi %*% X %*% Q %*% XtVi
      PZtZ <- P %*% Z %*% t(Z)
      Py <- P %*% y
      # Bentuk lain : Py <- Vi %*% (y - X %*% BETA)

      # Partial derivative of restricted log-likelihood with respect to sigma2_u
      # (Rao & Molina, 2015) after equation 5.2.21
      s <- -0.5 * sum(diag(PZtZ)) + 0.5 * t(y) %*% PZtZ %*% Py

      # Matrix of expected second derivaties of - restricted log likehood
      # with respect to sigma2 u (Rao & Molina, 2015) equation 5.2.22
      Isigma2_u <- sum(diag(PZtZ %*% PZtZ)) / 2

      k <- k + 1
      # (Rao & Molina, 2015) equation 5.2.18
      sigma2_u[k + 1] <- sigma2_u[k] + s / Isigma2_u
      diff <- abs(sigma2_u[k + 1] - sigma2_u[k]) / sigma2_u[k]
    }
  }

  sigma2_u <- max(sigma2_u[k + 1], 0)
  result$fit$random_effect_var <- sigma2_u

  # jika tidak konvergen
  if (k >= maxiter && diff >= precision) {
    result$fit$convergence <- FALSE
    cli::cli_alert_danger("After {maxiter} iterations, there is no convergence.")
    return(result)
  }


  # Beta estimation -------------------------------
  # random effect varians
  G <- diag(sigma2_u, m)
  # y varians
  V <- Z %*% G %*% t(Z) + R
  Vi <- solve(V)
  XtVi <- Xt %*% Vi
  Q <- solve(XtVi %*% X)
  BETA <- Q %*% XtVi %*% y

  # std error, pvalue beta, and eblup estimation --
  # varA <- 1 / Isigma2_u
  stderr_beta <- sqrt(diag(Q))
  zvalue <- BETA / stderr_beta
  pvalue <- 2 * stats::pnorm(abs(zvalue), lower.tail = FALSE)
  coef_est <- data.frame(BETA, stderr_beta, zvalue, pvalue)
  colnames(coef_est) <- c("beta", "Std.Error", "z-value", "p-value")

  Xbeta <- X %*% BETA
  resid <- y - Xbeta
  u <- (sigma2_u / (sigma2_u + vardir)) * resid
  u <- as.vector(u)
  eblup_est <- Xbeta + u

  # Goodness ------------------------------
  loglike <- -0.5 * (sum(log(2 * pi * (sigma2_u + vardir)) + (resid^2) / (sigma2_u + vardir)))
  AIC <- -2 * loglike + 2 * (p + 1)
  BIC <- -2 * loglike + (p + 1) * log(m)

  # MSE package sae -----------------------------------
  g1d <- g2d <- g3d <- mse2d <- rep(0, m)
  Bd <- vardir / (sigma2_u + vardir)
  SumAD2 <- sum(diag(Vi)^2)
  VarA <- 2 / SumAD2

  if (method == "ML") {
    b <- -1 * sum(diag(Q %*% (t((Vi %*% Vi) %*% X) %*% X))) / SumAD2
    for (d in 1:m) {
      g1d[d] <- vardir[d] * (1 - Bd[d])
      xd <- matrix(X[d, ], nrow = 1, ncol = p)
      g2d[d] <- (Bd[d]^2) * xd %*% Q %*% t(xd)
      g3d[d] <- (Bd[d]^2) * VarA / (sigma2_u + vardir[d])
      mse2d[d] <- g1d[d] + g2d[d] + 2 * g3d[d] - b * (Bd[d]^2)
    }
  } else if (method == "REML") {
    for (d in 1:m) {
      g1d[d] <- vardir[d] * (1 - Bd[d])
      xd <- matrix(X[d, ], nrow = 1, ncol = p)
      g2d[d] <- (Bd[d]^2) * xd %*% Q %*% t(xd)
      g3d[d] <- (Bd[d]^2) * VarA / (sigma2_u + vardir[d])
      mse2d[d] <- g1d[d] + g2d[d] + 2 * g3d[d]
    }
  }
  # MSE with matrix -----------------------------------
  # g1 <- G - G %*% t(Z) %*% Vi %*% Z %*% G
  # aT <- Z %*% G %*% t(Z) %*% Vi
  # dT <- X - aT %*% X
  # g2 <- dT %*% solve(Xt %*% Vi %*% X) %*% t(dT)
  # g2 <- dT %*% Q %*% t(dT)

  df_res$random_effect <- u
  df_res$eblup <- eblup_est
  df_res$g1 <- g1d
  df_res$g2 <- g2d
  df_res$g3 <- g3d
  df_res$vardir <- vardir
  df_res$mse <- mse2d
  df_res$rse <- sqrt(df_res$mse) * 100 / df_res$eblup

  result$df_res <- df_res
  result$fit$estcoef <- coef_est
  result$fit$goodness <- c(loglike = loglike, AIC = AIC, BIC = BIC)
  result$fit$n_iter <- k
  class(result) <- "eblupres"

  if (print_result) {
    cli::cli_alert_success("Convergence after {.orange {k}} iterations")
    cli::cli_alert("Method : {method}")
    cli::cli_h1("Coefficient")
    stats::printCoefmat(result$fit$estcoef, signif.stars = TRUE)
  }

  return(invisible(result))
}





# Fungsi Penolong ---------------------------------------------------------

# extract variable from data frame
.get_variable <- function(data, variable) {
  if (length(variable) == nrow(data)) {
    return(variable)
  } else if (methods::is(variable, "character")) {
    if (variable %in% colnames(data)) {
      variable <- data[[variable]]
    }else{
      cli::cli_abort('variable "{variable}" is not found in the data')
    }
  } else if (methods::is(variable, "formula")) {
    # extract column name (class character) from formula
    variable <- data[[all.vars(variable)]]
  } else {
    cli::cli_abort('variable "{variable}" is not found in the data')
  }
  return(variable)
}

.get_value <- function(x, klas, ns, fun = mean) {
  agg_klas <- stats::aggregate(x, list(klas[!ns]), FUN = fun, na.rm = TRUE)
  x_ns <- dplyr::left_join(
    data.frame(Group.1 = klas[ns]) ,
    agg_klas, by = 'Group.1'
  )$x
  return(x_ns)
}
