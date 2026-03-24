#' Tidy a dma_result object
#'
#' Extract estimates, standard errors, and confidence intervals from
#' a dma_result object into a tidy data frame.
#'
#' @param x An object of class \code{dma_result}.
#' @param conf.level Confidence level for intervals (default 0.95).
#' @param ... Additional arguments (ignored).
#'
#' @return A data.frame with columns: term, estimate, std.error, conf.low, conf.high.
#' @export
tidy.dma_result <- function(x, conf.level = 0.95, ...) {
	rows <- lapply(names(x$estimates), function(nm) {
		est <- x$estimates[[nm]]
		data.frame(
			term = nm,
			estimate = est@x,
			std.error = est@std_error,
			conf.low = est@conf_int[1],
			conf.high = est@conf_int[2],
			stringsAsFactors = FALSE
		)
	})
	do.call(rbind, rows)
}
