#' Benchmarking Procedure for EBLUPs based on a Fay-Herriot Model.
#'
#' @references
#' \enumerate{
#'  \item Rao & Molina (2015). Small area estimation. John Wiley & Sons.
#' }
#'
#' @param formula An object of class formula that contains a description of the model to be fitted. The variables included in the formula must be contained in the data.
#' @param data A data frame or a data frame extension (e.g. a tibble)
#' @param vardir Vector or column names from data that contain variance sampling from the direct estimator for each area
#' @param weight Vector or column names from data that contain areas weights, in case of proportions it denotes the population shares.
#' @param method The benchmarking method to be specified among "Ratio", "Difference", "Optimal_vardir", "Optimal_mse", "Optimal_covariance", and "You-Rao" see details.
#' @param ... Further arguments passed to \code{eblupfh}.
#'
#' @details
#' The function allows performing four different benchmarking methods, according to the argument method.
#' \enumerate{
#'  \item The Ratio Benchmarking (\code{Ratio}) method is one of the simplest benchmarking methods. The estimate is obtained by multiplying the EBLUP estimate with a common adjustment factor \deqn{\frac{\sum_{l=1}^{m} W_{l} \hat{\theta}_{l}}{\sum_{l=1}^{m} W_{l} \hat{\theta}_{Hl}}}
#'  The Ratio Benchmarking method has several limitations: (i) a common adjustment factor is applied to all estimators, regardless of their precision. (ii) Additionally, the estimation from ratio benchmarking is not design-consistent, as the sample size in area-i increases while the sample sizes in the remaining areas are held fixed. (iii) Furthermore, second-order unbiased MSE estimators are not readily available (Pfeffermann, Sikov, and Tiller, 2014).
#'  \item The Difference Benchmarking (\code{Difference}) method.
#'  The Difference Benchmarking estimate is obtained through the following formula \deqn{\hat{\theta}^{DB}_{i} = \hat{\theta}^{H}_{i} + \left( \sum_{l=1}^{m} W_l \hat{\theta}_l - \sum_{l=1}^{m} W_l \hat{\theta}^{H}_{l} \right)}
#'  The Difference Benchmarking method has some limitations similar to those of ratio benchmarking; however, it permits second-order unbiased MSE estimation.
#'  \item The Optimal Benchmarking or Wang, Fuller, and Qu (WFQ Benchmarking) (\code{Optimal}) method does not suffer from the limitations (i) and (ii). The optimal benchmarking estimate is obtained through the following matrix formula
#'  \deqn{\tilde{\theta}^{OMB} = \tilde{\theta}^H + \Omega^{-1} W \left( W^T \Omega^{-1} W \right)^{-1} \left( \hat{\theta}^+ - W^T \tilde{\theta}^H \right)}
#'  This estimator depends on the choice of \eqn{\Omega}, popular choices of \eqn{\Omega^{-1}} include (i) \eqn{\Omega^{-1} = diag(\psi_1, \dots, \psi_m)} (\code{vardir}), (ii) \eqn{\Omega^{-1} = MSE(\hat{\theta}^H)} (\code{mse}), and (iii) \eqn{\Omega^{-1} = W^{-1} Cov(\hat{\theta}^{H}, W\hat{\theta}^{H})} (\code{covariance}).
#'  \item The You-Rao Benchmarking (\code{Self} or \code{You-Rao}) method. The two methods are self-benchmarking (or self-calibrating) in the sense that adjustments are not made to EBLUP estimators; instead, estimators that automatically satisfy the benchmarking constraints are obtained.
#'  Method 1 replaces the optimal \eqn{\hat{\beta}} in the EBLUP estimator \eqn{\hat{\theta}^{H}_{i}} with an estimator that depends on the benchmarking weights \eqn{W_l}.
#'  Method 2, on the other hand, augments the vector \eqn{z_i^T} of covariates to \eqn{(z_i^T, W_i \psi_i)} and uses the EBLUP estimator of \eqn{\theta_i} based
#'  on the augmented model. This two methods does not suffer from the limitations (i)-(iii)
#' }
#' @return The function produces the same result in both the \code{eblupfh} and \code{eblupfh_cluster} functions, returning a list containing the objects (\code{df_res} and \code{fit}). However, in \code{df_res}, additional columns for benchmark estimates and relative standard errors (RSE) are included as part of the benchmark estimation output.
#' @export
#'
#' @examples
#' library(saens)
#'
#' df_mys <- na.omit(mys)
#'
#' model_rb <- eblupfh_benchmark(
#'   y ~ x1 + x2 + x3, data = df_mys,
#'   vardir = "var", weight = "weight",
#'   method = "ratio"
#' )
#' model_db <- eblupfh_benchmark(
#'   y ~ x1 + x2 + x3, data = df_mys,
#'   vardir = "var", weight = "weight",
#'   method = "Difference"
#' )
#' model_wfq <- eblupfh_benchmark(
#'   y ~ x1 + x2 + x3, data = df_mys,
#'   vardir = "var", weight = "weight",
#'   method = "Optimal_vardir"
#' )
#' model_yr <- eblupfh_benchmark(
#'   y ~ x2 + x3, data = df_mys,
#'   vardir = "var", weight = "weight",
#'   method = "You-Rao"
#' )
#' @md
eblupfh_benchmark <- function(formula, vardir, method, weight, data, ...) {
  method <- tolower(method)
  if (!method %in% c("ratio", "difference", "optimal_vardir", "optimal_mse", "optimal_covariance", "you-rao")) {
    cli::cli_abort('The method should be one of the following: "Ratio", "Difference", "Optimal_vardir", "Optimal_MSE", "Optimal_covariance", or "You-Rao"')
  }

  if (method != "you-rao") {
    # y <- stats::model.frame(formula, data, na.action = NULL)[[1]]
    # if(any(is.na(y))){
    #   model <- saens::eblupfh_cluster(
    #     formula = formula, data = data, vardir = vardir, print_result = print_result, ...
    #   )
    # }else{
    #   model <- saens::eblupfh(
    #     formula = formula, data = data, vardir = vardir, print_result = print_result, ...
    #   )
    # }

    model <- saens::eblupfh(
      formula = formula, data = data, vardir = vardir, ...
    )

    y <- model$df_res$y
    eblup <- model$df_res$eblup
    weight <- .get_variable(data, weight)
    vardir <- model$df_res$vardir
    refvar <- model$fit$random_effect_var
    method <- tolower(method)

    if (method %in% c("ratio")) {
      ratio_benchmark <- sum(weight * y) / sum(weight * eblup)
      model$df_res <- dplyr::mutate(
        model$df_res,
        eblup_RB = eblup * ratio_benchmark,
        .after = "eblup"
      )
    } else if (method %in% c("difference")) {
      # Estimation
      alfa <- sum(weight * y) - sum(weight * eblup)
      model$df_res <- dplyr::mutate(
        model$df_res,
        eblup_DB = eblup + alfa,
        .after = "eblup"
      )

      # MSE
      X <- stats::model.matrix(formula, data)
      Vi <- 1 / (refvar + vardir)
      XtVi <- t(Vi * X)
      Q <- solve(XtVi %*% X)
      p <- ncol(X)
      Bd <- vardir / (refvar + vardir)

      g4.a <- sum(weight^2 * Bd^2 / (Vi))
      g4.b <- 0
      for (i in 1:p) {
        xdi <- matrix(X[i, ], nrow = 1, ncol = p)
        for (j in 1:length(weight)) {
          xdj <- matrix(X[j, ], nrow = 1, ncol = p)
          g4.b <- g4.b + weight[i] * weight[j] * Bd[i] * Bd[j] * (xdi %*% Q %*% t(xdj))
        }
      }

      g4 <- g4.a + g4.b

      model$df_res$mse_DB <- model$df_res$mse + as.numeric(g4)
      model$df_res$rse_DB <- sqrt(model$df_res$mse_DB) * 100 / model$df_res$eblup_DB

    } else if (grepl("optimal", method)) {
      # Estimation
      if (grepl("vardir", method)) {
        omega <- vardir
      } else if (grepl("mse", method)) {
        omega <- model$df_res$mse
      } else if (grepl("covariance", method)) {
        cov_temp <- stats::cov(eblup, rep(sum(eblup * weight), length(weight)))
        omega <- rep(cov_temp, length(weight)) / weight
      }

      weight2 <- weight^2
      lambda <- (omega * weight) / sum(weight2 * omega)
      alfa <- sum(weight * y) - sum(weight * eblup)

      model$df_res <- dplyr::mutate(
        model$df_res,
        eblup_OB = eblup + (lambda * alfa),
        .after = "eblup"
      )
    }
  } else if (method == "you-rao") {
    model <- eblupfh_yr(
      formula = formula, data = data, vardir = vardir, weight = weight
    )
  }

  return(model)
}


# eblup You Rao -----------------------------------------------------------
eblupfh_yr <- function(formula, data, vardir, weight, method = "REML",
                       maxiter = 100, precision = 1e-04, scale = FALSE,
                       print_result = TRUE) {
  y <- stats::model.frame(formula, data, na.action = NULL)[[1]]
  # data hasil
  df_res <- data.frame(
    y = y,
    vardir = NA,
    random_effect = NA,
    eblup = NA,
    # g1 = NA, g2 = NA, g3 = NA,
    mse = NA, rse = NA, eblup_yr = NA, mse_yr = NA, rse_yr = NA
  )


  if (any(is.na(y))) {
    cli::cli_abort("variable y contains NA values, please use eblupfh cluster function")
  }
  datas <- data

  vardir <- .get_variable(datas, vardir)
  weight <- .get_variable(datas, weight)
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


  # Perubahan 1 ----------------------------------------
  Bd <- vardir / (sigma2_u + vardir)
  XtBdW <- t(Bd * weight * X)
  Q2 <- solve(XtBdW %*% X)
  beta.REML2 <- Q2 %*% XtBdW %*% y
  # Perubahan 1 ----------------------------------------



  # std error, pvalue beta, and eblup estimation --
  # varA <- 1 / Isigma2_u
  stderr_beta <- sqrt(diag(Q))
  zvalue <- BETA / stderr_beta
  pvalue <- 2 * stats::pnorm(abs(zvalue), lower.tail = FALSE)
  coef_est <- data.frame(BETA, stderr_beta, zvalue, pvalue)
  colnames(coef_est) <- c("beta", "Std.Error", "z-value", "p-value")



  # Perubahan 2 ------------------------------------------
  Xbeta.REML2 <- X %*% beta.REML2
  resid2 <- y - Xbeta.REML2
  u2 <- (sigma2_u / (sigma2_u + vardir)) * resid2
  u2 <- as.vector(u2)
  eblup_est2 <- Xbeta.REML2 + u2
  # Perubahan 2 ------------------------------------------

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
  A <- sigma2_u
  g1d <- g2d <- g2d_yr <- g3d <- mse2d <- mseyr <- rep(0, m)
  # Bd <- vardir / (sigma2_u + vardir)
  SumAD2 <- sum(diag(Vi)^2)
  VarA <- 2 / SumAD2

  BdW <- Bd * weight
  WBdGamma <- weight^2 * Bd / (1 - Bd)
  BdWXtX <- ((t(X) * BdW) %*% X)
  g2.a <- solve(BdWXtX)
  g2.b <- ((t(X) * WBdGamma) %*% X)
  VB <- (A * g2.a) %*% g2.b %*% g2.a

  VarA <- 2 / SumAD2
  if (method == "REML") {
    for (d in 1:m) {
      g1d[d] <- vardir[d] * (1 - Bd[d])
      xd <- matrix(X[d, ], nrow = 1, ncol = p)

      # MSE EBLUP default
      g2d[d] <- (Bd[d]^2) * xd %*% Q %*% t(xd)
      # MSE EBLUP You-Rao
      g2d_yr[d] <- (Bd[d]^2) * xd %*% VB %*% t(xd)

      g3d[d] <- (Bd[d]^2) * VarA/(A + vardir[d])

      # MSE EBLUP default
      mse2d[d] <- g1d[d] + g2d[d] + 2 * g3d[d]
      # MSE EBLUP You-Rao
      mseyr[d] <- g1d[d] + g2d_yr[d] + 2 * g3d[d]
    }
  }
  # MSE with matrix -----------------------------------
  # g1 <- G - G %*% t(Z) %*% Vi %*% Z %*% G
  # aT <- Z %*% G %*% t(Z) %*% Vi
  # dT <- X - aT %*% X
  # g2 <- dT %*% solve(Xt %*% Vi %*% X) %*% t(dT)
  # g2 <- dT %*% Q %*% t(dT)

  df_res$random_effect <- u
  df_res$eblup <- as.vector(eblup_est)
  df_res$eblup_yr <- as.vector(eblup_est2)
  # df_res$g1 <- g1d
  # df_res$g2 <- g2d
  # df_res$g3 <- g3d
  df_res$vardir <- vardir
  df_res$mse <- mse2d
  df_res$rse <- sqrt(df_res$mse) * 100 / df_res$eblup
  df_res$mse_yr <- mseyr
  df_res$rse_yr <- sqrt(df_res$mse_yr) * 100 / df_res$eblup_yr

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




