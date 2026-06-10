#' Counterfactual CDF estimates by y
#'
#' For a fitted \code{dma_result} and a grid of y-values, computes the
#' distributional analog \eqn{F_{a,a'}(y) = P(Y(a, M(a')) \le y)} along with
#' pointwise standard errors and confidence intervals derived from the
#' y-indexed efficient influence function (see companion paper, \S5.1).
#'
#' The function reuses the trained engression conditional-distribution model and
#' the Riesz density-ratio nuisances from a fitted \code{dma_result}.  For each
#' threshold \code{y}, the conditional CDF nuisance
#' \eqn{\tilde\mu_y(a, m, w) = P(Y \le y \mid A=a, M=m, W=w)} is obtained from
#' the engression model by drawing \code{N_sim} samples per observation and
#' computing the empirical proportion below \code{y}.  The intermediate
#' iterated-expectation stages (\eqn{\tilde\theta_y} and its marginal) are
#' computed via ordinary least squares, following the same telescoping structure
#' as the mean EIF.  The propensity and mediator-density-ratio weights
#' (\code{alpha}) are unchanged because they do not depend on \eqn{Y}.
#'
#' @param fit  A \code{dma_result} returned by \code{\link{dma}}.
#' @param y    Numeric vector of thresholds at which to evaluate \eqn{F}.
#' @param level Confidence level for pointwise intervals (default 0.95).
#' @param N_sim Number of engression samples per observation used to approximate
#'   \eqn{P(Y \le y \mid \cdot)}.  Default 200.
#' @param jkl  Character string identifying which (a, a') regime to evaluate.
#'   Must be one of \code{colnames(fit$alpha_n[[1]])} (for natural/organic/RT
#'   effects) or \code{colnames(fit$alpha_r[[1]])} (for RI effects).  Required
#'   whenever more than one regime is stored (the stored column order is not
#'   guaranteed, so a default would silently select an arbitrary regime); may
#'   be omitted only when exactly one regime exists.
#'
#' @return A \code{data.frame} with columns:
#' \describe{
#'   \item{y}{The input threshold values.}
#'   \item{Fhat}{Point estimate \eqn{\hat F_{a,a'}(y)}.}
#'   \item{std_error}{Pointwise standard error.}
#'   \item{conf_low}{Lower bound of the pointwise \code{level}-confidence interval.}
#'   \item{conf_high}{Upper bound of the pointwise confidence interval.}
#' }
#'
#' @export
F_hat <- function(fit, y, level = 0.95, N_sim = 200L, jkl = NULL) {

  # ---- input checks --------------------------------------------------------
  if (!inherits(fit, "dma_result")) {
    stop("'fit' must be a dma_result object returned by dma().")
  }
  if (!is.numeric(y) || length(y) == 0) {
    stop("'y' must be a non-empty numeric vector.")
  }
  if (!is.numeric(level) || length(level) != 1L || level <= 0 || level >= 1) {
    stop("'level' must be a single number in (0, 1).")
  }
  N_sim <- as.integer(N_sim)
  if (N_sim < 1L) stop("'N_sim' must be a positive integer.")

  # ---- identify which alpha block to use -----------------------------------
  use_natural <- length(fit$alpha_n) > 0
  alphas <- if (use_natural) fit$alpha_n else fit$alpha_r

  if (length(alphas) == 0) {
    stop("No density ratios found in the result object.")
  }

  regime_cols <- colnames(alphas[[1]])
  if (is.null(jkl)) {
    if (length(regime_cols) > 1L) {
      stop(sprintf(
        "F_hat(): 'jkl' is required when more than one regime is stored. Available regimes: %s",
        paste(regime_cols, collapse = ", ")
      ))
    }
    jkl <- regime_cols[1L]
  }
  if (!jkl %in% regime_cols) {
    stop(sprintf("'jkl' must be one of: %s", paste(regime_cols, collapse = ", ")))
  }

  # ---- extract stored objects ----------------------------------------------
  data      <- fit$data
  vars      <- fit$vars
  folds     <- fit$folds
  models_y  <- fit$models_y
  n         <- nrow(data)

  Y_obs     <- data[[vars@Y]]

  # variable groups
  x_vars <- na.omit(c(vars@A, vars@W, vars@M, vars@Z))
  W_vars <- vars@W
  A_vars <- vars@A
  AW_vars <- c(A_vars, W_vars)

  # reference column names from engression (use first fold validation data)
  valid1   <- data[folds[[1]]$validation_set, , drop = FALSE]
  ref_cols <- colnames(prepare_engression_x(valid1, x_vars))

  # ---- build shifted data frames (full n rows) -----------------------------
  cens_col <- vars@C
  d0_fn    <- fit$d0
  d1_fn    <- fit$d1

  data_0 <- shift_data(data, A_vars, cens_col, d0_fn)
  data_1 <- shift_data(data, A_vars, cens_col, d1_fn)

  # ---- decode which shifted data variants correspond to this jkl ----------
  jkl_chars <- strsplit(jkl, "")[[1]]

  get_shifted_by_char <- function(ch) {
    if (ch == "0") return(data_0)
    if (ch == "1") return(data_1)
    data
  }

  if (use_natural) {
    # 3-char jkl: j, k, l
    shifted_j <- get_shifted_by_char(jkl_chars[1])   # data used for mu_tilde on M arm
    shifted_k <- get_shifted_by_char(jkl_chars[2])   # data used for b3 prediction
    shifted_l <- get_shifted_by_char(jkl_chars[3])   # data used for b2 prediction
  } else {
    # 4-char jkl: i, j, k, l  (RI)
    shifted_i <- get_shifted_by_char(jkl_chars[1])
    shifted_j <- get_shifted_by_char(jkl_chars[2])
    shifted_k <- get_shifted_by_char(jkl_chars[3])
    shifted_l <- get_shifted_by_char(jkl_chars[4])
  }

  # ---- draw N_sim samples from engression per fold -------------------------
  draw_samples <- function(df_list) {
    # df_list: list of data frames, one per fold (validation set rows)
    lapply(seq_along(folds), function(v) {
      valid_idx <- folds[[v]]$validation_set
      X <- prepare_engression_x(df_list[[v]], x_vars, ref_cols)
      raw <- predict(models_y[[v]], X, type = "sample", nsample = N_sim)
      matrix(raw, nrow = length(valid_idx), ncol = N_sim)
    })
  }

  # fold-indexed subsets of each shifted dataset
  fold_subset <- function(df) lapply(folds, function(f) df[f$validation_set, , drop = FALSE])

  # Only two engression draws enter the EIF: the observed data (for
  # mu_tilde_obs) and the top-stage shift -- j for natural effects, i for RI.
  # The lower cascade stages (k, l shifts) are OLS predictions of these draws,
  # so no additional engression sampling is needed for them.
  samp_obs_list <- draw_samples(fold_subset(data))
  if (use_natural) {
    samp_j_list <- draw_samples(fold_subset(shifted_j))
  } else {
    samp_i_list <- draw_samples(fold_subset(shifted_i))
  }

  # ---- reassemble in original row order ------------------------------------
  all_valid_idx <- Reduce(c, lapply(folds, function(f) f$validation_set))
  ord           <- order(all_valid_idx)

  reorder_rows <- function(lst) {
    do.call(rbind, lst)[ord, , drop = FALSE]
  }

  samp_obs <- reorder_rows(samp_obs_list)
  if (use_natural) {
    samp_j <- reorder_rows(samp_j_list)
  } else {
    samp_i <- reorder_rows(samp_i_list)
  }

  # ---- alpha weights (n-vectors for the selected jkl column) ---------------
  alph1 <- alphas$alpha1[, jkl]
  alph2 <- alphas$alpha2[, jkl]
  alph3 <- alphas$alpha3[, jkl]
  if (!use_natural) alph4 <- alphas$alpha4[, jkl]

  # ---- design matrices for cascade OLS regressions -------------------------
  # These are fixed across y-values (only the response changes per y)
  # We use one-hot-encoded versions of the factor variables for robustness
  X_AW     <- as.matrix(one_hot_encode(data,       AW_vars))
  X_AW_k   <- as.matrix(one_hot_encode(shifted_k,  AW_vars))
  X_W      <- as.matrix(one_hot_encode(data,        W_vars))
  X_W_l    <- as.matrix(one_hot_encode(shifted_l,   W_vars))

  if (!use_natural) {
    AMW_vars <- na.omit(c(A_vars, vars@M, W_vars))
    X_AMW    <- as.matrix(one_hot_encode(data,      AMW_vars))
    X_AMW_j  <- as.matrix(one_hot_encode(shifted_j, AMW_vars))
    X_AW_obs <- X_AW
  }

  # safe lm.fit wrapper: returns coefficients or NULL on error
  safe_lm <- function(X, y_resp) {
    tryCatch(
      stats::lm.fit(cbind(1, X), y_resp)$coefficients,
      error = function(e) NULL
    )
  }

  safe_predict <- function(coef, X) {
    if (is.null(coef)) return(NULL)
    as.vector(cbind(1, X) %*% coef)
  }

  z_crit <- stats::qnorm((1 + level) / 2)

  # ---- loop over y values --------------------------------------------------
  results <- lapply(y, function(yval) {

    # conditional CDF at each observation (n-vectors)
    mu_tilde_obs <- rowMeans(samp_obs <= yval)

    # indicator outcome
    Y_ind <- as.numeric(!is.na(Y_obs) & Y_obs <= yval)
    mult  <- as.numeric(!is.na(Y_obs))

    if (use_natural) {
      # -------------------------------------------------------------------
      # Natural effect cascade (3-level), mirroring eif_n in eif.R:
      #   fit3_natural = mu_tilde_obs  (P(Y<=y | obs (A,M,W)))
      #   b3           = mu_tilde_j    (P(Y<=y | A<-j, obs (M,W)))
      #   fit2_natural = E[mu_tilde_j | (A,W)] predicted at obs data
      #   b2           = E[mu_tilde_j | (A,W)] predicted at data_k
      #   fit1_natural = E[b2 | W] predicted at obs data
      #   b1           = E[b2 | W] predicted at data_l (per observation)
      # -------------------------------------------------------------------
      mu_tilde_j <- rowMeans(samp_j <= yval)

      # stage 2: lm(mu_tilde_j ~ A + W_ohe), predict at data_k and obs data
      coef2      <- safe_lm(X_AW, mu_tilde_j)
      b3_k       <- safe_predict(coef2, X_AW_k)
      fit2_nat   <- safe_predict(coef2, X_AW)
      if (is.null(b3_k)) {
        b3_k     <- rep(mean(mu_tilde_j, na.rm = TRUE), n)
        fit2_nat <- b3_k
      }

      # stage 1: lm(b3_k ~ W_ohe), predict at data_l and obs data
      coef1      <- safe_lm(X_W, b3_k)
      b2_l       <- safe_predict(coef1, X_W_l)
      fit1_nat   <- safe_predict(coef1, X_W)
      if (is.null(b2_l)) {
        b2_l     <- rep(mean(b3_k, na.rm = TRUE), n)
        fit1_nat <- b2_l
      }

      # EIF (mirrors eif_n with indicator outcome and CDF nuisances):
      # alpha3 * mult * (Y_ind - fit3_natural) +
      # alpha2 * (b3 - fit2_natural)           +   b3 is the j-shifted draw
      # alpha1 * (b2 - fit1_natural)           +
      # b1                                         per observation, not E[b1]
      eif_vals <- alph3 * mult * (Y_ind - mu_tilde_obs) +
                  alph2 * (mu_tilde_j   - fit2_nat)     +
                  alph1 * (b3_k         - fit1_nat)     +
                  b2_l

    } else {
      # -------------------------------------------------------------------
      # RI effect cascade (4-level):
      #   fit4_natural = mu_tilde_i predicted at obs data
      #   b4           = E[mu_tilde_i | (A,M,W)] predicted at data_j
      #   fit3_natural = E[mu_tilde_i | (A,M,W)] predicted at obs data
      #   b3           = E[b4 | (A,W)] predicted at data_k
      #   fit2_natural = E[b4 | (A,W)] predicted at obs data
      #   b2           = E[b3 | W] predicted at data_l
      #   fit1_natural = E[b3 | W] predicted at obs data
      #   b1           = E[b3 | W] predicted at data_l (per observation)
      # -------------------------------------------------------------------
      mu_tilde_i <- rowMeans(samp_i <= yval)

      coef3      <- safe_lm(X_AMW, mu_tilde_i)
      b4_j       <- safe_predict(coef3, X_AMW_j)
      fit3_nat   <- safe_predict(coef3, X_AMW)
      if (is.null(b4_j)) {
        b4_j     <- rep(mean(mu_tilde_i, na.rm = TRUE), n)
        fit3_nat <- b4_j
      }

      coef2      <- safe_lm(X_AW, b4_j)
      b3_k       <- safe_predict(coef2, X_AW_k)
      fit2_nat   <- safe_predict(coef2, X_AW)
      if (is.null(b3_k)) {
        b3_k     <- rep(mean(b4_j, na.rm = TRUE), n)
        fit2_nat <- b3_k
      }

      coef1      <- safe_lm(X_W, b3_k)
      b2_l       <- safe_predict(coef1, X_W_l)
      fit1_nat   <- safe_predict(coef1, X_W)
      if (is.null(b2_l)) {
        b2_l     <- rep(mean(b3_k, na.rm = TRUE), n)
        fit1_nat <- b2_l
      }

      # Mirrors eif_r: the alpha3 term uses the i-shifted draw (b4 = mu_tilde_i),
      # and the final term is the per-observation l-shifted prediction.
      eif_vals <- alph4 * mult * (Y_ind - mu_tilde_obs) +
                  alph3 * (mu_tilde_i - fit3_nat)       +
                  alph2 * (b4_j       - fit2_nat)       +
                  alph1 * (b3_k       - fit1_nat)       +
                  b2_l
    }

    Fhat_y <- mean(eif_vals, na.rm = TRUE)
    n_obs  <- sum(!is.na(eif_vals))
    se_y   <- if (n_obs > 1) stats::sd(eif_vals, na.rm = TRUE) / sqrt(n_obs) else NA_real_

    data.frame(
      y         = yval,
      Fhat      = Fhat_y,
      std_error = se_y,
      conf_low  = Fhat_y - z_crit * se_y,
      conf_high = Fhat_y + z_crit * se_y
    )
  })

  do.call(rbind, results)
}
