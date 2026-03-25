#' dma result object
#'
#' @param estimates A list of effect estimates.
#' @param outcome_reg Outcome regression predictions.
#' @param alpha_n Natural density ratio estimates.
#' @param alpha_r Randomized density ratio estimates.
#' @param models_y List of trained engression models.
#' @param folds List of cross-fit folds.
#' @param vars Object of class dma_vars.
#' @param data Data frame used for estimation.
#' @param call The matched call.
#' @param effect The estimated effect type.
#'
#' @return An object of class \code{dma_result}.
new_dma_result <- function(estimates, outcome_reg, alpha_n, alpha_r,
						   models_y, folds, vars, data, call, effect, d0, d1) {
	structure(
		list(
			estimates = estimates,
			outcome_reg = outcome_reg,
			alpha_n = alpha_n,
			alpha_r = alpha_r,
			models_y = models_y,
			folds = folds,
			vars = vars,
			data = data,
			call = call,
			effect = effect,
			d0 = d0,
			d1 = d1
		),
		class = "dma_result"
	)
}

#' @export
plot.dma_result <- function(x, ...) {
	if (!requireNamespace("ggplot2", quietly = TRUE)) {
		stop("Package 'ggplot2' is required for plotting.")
	}

	est_names <- names(x$estimates)
	res_df <- do.call(rbind, lapply(est_names, function(nm) {
		est <- x$estimates[[nm]]
		data.frame(
			term = nm,
			estimate = est@x,
			conf.low = est@conf_int[1],
			conf.high = est@conf_int[2]
		)
	}))

	res_df$term <- factor(res_df$term, levels = rev(est_names))

	ggplot2::ggplot(res_df, ggplot2::aes(x = estimate, y = term)) +
		ggplot2::geom_vline(xintercept = 0, linetype = "dashed", color = "grey50") +
		ggplot2::geom_point(size = 3, color = "steelblue") +
		ggplot2::geom_errorbarh(ggplot2::aes(xmin = conf.low, xmax = conf.high),
								height = 0.2, color = "steelblue", linewidth = 1) +
		ggplot2::labs(title = paste("dma Estimates:", x$effect),
					  x = "Estimate", y = "") +
		ggplot2::theme_minimal()
}
