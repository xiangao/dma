#' Plot counterfactual outcome densities
#'
#' Visualizes the distribution of the outcome under different treatment regimes
#' using the trained engression models from cross-fitting.
#'
#' @param result An object of class \code{dma_result}.
#' @param n_samples Number of samples to draw per observation (default 1).
#' @param regimes A named list of shift functions or numeric values for treatment.
#'  Default uses the d0/d1 from the original call.
#'
#' @return A ggplot2 object.
#' @export
plot_counterfactual_density <- function(result, n_samples = 1, regimes = NULL) {
	if (!requireNamespace("ggplot2", quietly = TRUE)) {
		stop("Package 'ggplot2' is required for plotting.")
	}

	data <- result$data
	vars <- result$vars
	models <- result$models_y
	folds <- result$folds

	if (length(models) == 0) {
		stop("No trained models found in the result object.")
	}

	if (is.null(regimes)) {
		regimes <- list(
			"Regime 0" = result$call$d0,
			"Regime 1" = result$call$d1
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
		sample_list <- vector("list", length(folds))

		for (v in seq_along(folds)) {
			valid_idx <- folds[[v]]$validation_set
			if (length(valid_idx) == 0) next

			X_valid <- prepare_engression_x(data_shifted[valid_idx, , drop = FALSE], x_vars)
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
					  subtitle = paste("Effect Type:", result$effect),
					  x = "Outcome (Y)", y = "Density") +
		ggplot2::theme_minimal()
}
