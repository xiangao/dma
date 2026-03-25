#' Plot counterfactual outcome densities
#'
#' Visualizes the distribution of the outcome under different treatment regimes.
#' Two modes: (1) shift-and-predict using trained engression models (default),
#' which shows marginal outcome distributions P(Y|do(A=a)); (2) weighted
#' densities using the learned Riesz representers (\code{use_weights = TRUE}),
#' which shows all mediation counterfactual regimes including cross-world
#' quantities like Y(1, M(0)).
#'
#' @param result An object of class \code{dma_result}.
#' @param n_samples Number of samples to draw per observation (for shift-and-predict).
#' @param regimes A named list of shift functions or numeric values for treatment.
#'  Default uses the d0/d1 from the original call. Only used when
#'  \code{use_weights = FALSE}.
#' @param use_weights If \code{TRUE}, uses the learned density ratios (Riesz
#'  representers) to estimate counterfactual distributions via importance
#'  weighting. This shows all mediation-relevant regimes, not just the two
#'  marginal treatment regimes. Default is \code{FALSE}.
#'
#' @return A ggplot2 object.
#' @export
plot_counterfactual_density <- function(result, n_samples = 1, regimes = NULL, use_weights = FALSE) {
	if (!requireNamespace("ggplot2", quietly = TRUE)) {
		stop("Package 'ggplot2' is required for plotting.")
	}

	data <- result$data
	vars <- result$vars

	if (use_weights) {
		alphas <- if (length(result$alpha_n) > 0) result$alpha_n else result$alpha_r
		if (length(alphas) == 0) {
			stop("No density ratios found in the result object.")
		}

		final_alpha <- if ("alpha4" %in% names(alphas)) "alpha4" else "alpha3"
		W <- alphas[[final_alpha]]

		regime_labels <- c(
			# Natural
			"000" = "Y(0, M(0))", "100" = "Y(1, M(0))", "111" = "Y(1, M(1))",
			# Organic
			"101" = "Y(1, M_obs)",
			# RT natural extras
			"011" = "Y(0, M(1), Z(1))", "010" = "Y(0, M(1), Z(0))",
			# Randomized
			"0000" = "Y(0, M_pop(0))", "1100" = "Y(1, M_pop(0))", "1111" = "Y(1, M_pop(1))",
			# RT randomized
			"0011" = "Y(0, M(1), Z_pop(1))", "0010" = "Y(0, M(1), Z_pop(0))",
			"0111" = "Y(0, M(1), Z_pop(1))"
		)

		plot_list <- list()
		for (nm in colnames(W)) {
			w <- pmax(W[, nm], 0)
			w <- w / sum(w) * length(w)
			label <- if (nm %in% names(regime_labels)) regime_labels[[nm]] else nm
			plot_list[[nm]] <- data.frame(
				Y = data[[vars@Y]],
				Weight = w,
				Regime = label
			)
		}
		plot_df <- do.call(rbind, plot_list)

		return(
			ggplot2::ggplot(plot_df, ggplot2::aes(x = Y, fill = Regime, weight = Weight)) +
				ggplot2::geom_density(alpha = 0.5) +
				ggplot2::labs(title = "Counterfactual Outcome Densities (Weighted)",
							  subtitle = paste("Effect Type:", result$effect),
							  x = "Outcome (Y)", y = "Density") +
				ggplot2::theme_minimal()
		)
	}

	# Shift-and-predict mode
	models <- result$models_y
	folds <- result$folds

	if (length(models) == 0) {
		stop("No trained models found in the result object.")
	}

	if (is.null(regimes)) {
		regimes <- list(
			"Regime 0" = result$d0,
			"Regime 1" = result$d1
		)
	}

	all_samples <- list()

	for (reg_nm in names(regimes)) {
		d_func <- regimes[[reg_nm]]

		# Shift treatment
		data_shifted <- data
		if (is.function(d_func)) {
			data_shifted[[vars@A]] <- d_func(data, vars@A)
		} else {
			data_shifted[[vars@A]] <- d_func
		}

		# Predict using cross-fit models on their respective validation sets
		x_vars <- na.omit(c(vars@A, vars@W, vars@M, vars@Z))
		ref_cols <- x_vars  # preserve all columns including constant (shifted) ones
		sample_list <- vector("list", length(folds))

		for (v in seq_along(folds)) {
			valid_idx <- folds[[v]]$validation_set
			if (length(valid_idx) == 0) next

			X_valid <- prepare_engression_x(data_shifted[valid_idx, , drop = FALSE], x_vars, ref_cols)
			samples <- predict(models[[v]], X_valid, type = "sample", nsample = n_samples)
			sample_list[[v]] <- as.vector(samples)
		}

		all_samples[[reg_nm]] <- data.frame(
			Y = unlist(sample_list),
			Regime = reg_nm
		)
	}

	plot_df <- do.call(rbind, all_samples)

	ggplot2::ggplot(plot_df, ggplot2::aes(x = Y, fill = Regime)) +
		ggplot2::geom_density(alpha = 0.5) +
		ggplot2::labs(title = "Counterfactual Outcome Densities",
					  subtitle = paste("Effect Type:", result$effect, "(Marginal Shifts Only)"),
					  x = "Outcome (Y)", y = "Density") +
		ggplot2::theme_minimal()
}
