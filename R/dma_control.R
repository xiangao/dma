#' Control parameters for dma
#'
#' @param crossfit_folds [\code{numeric(1)}]\cr Number of crossfit folds.
#' @param zprime_folds [\code{numeric(1)}]\cr Number of folds for calculating Z'.
#' @param noise_dim [\code{numeric(1)}]\cr Engression noise dimension.
#' @param hidden_dim [\code{numeric(1)}]\cr NN hidden layer dimension.
#' @param num_layer [\code{numeric(1)}]\cr Number of NN layers for engression.
#' @param num_epochs [\code{numeric(1)}]\cr Training epochs for outcome regressions.
#' @param lr [\code{numeric(1)}]\cr Learning rate for outcome regressions.
#' @param riesz_epochs [\code{numeric(1)}]\cr Training epochs for Riesz representer.
#' @param riesz_lr [\code{numeric(1)}]\cr Learning rate for Riesz representer.
#' @param batch_size [\code{numeric(1)}]\cr Batch size for outcome regressions.
#' @param riesz_batch_size [\code{numeric(1)}]\cr Batch size for Riesz representer.
#' @param device [\code{character(1)}]\cr Torch device.
#'
#' @return A list of control parameters.
#' @export
dma_control <- function(crossfit_folds = 10L,
								zprime_folds = 1L,
								noise_dim = 5L,
								hidden_dim = 100L,
								num_layer = 3L,
								num_epochs = 500L,
								lr = 1e-3,
								riesz_epochs = 100L,
								riesz_lr = 0.01,
								batch_size = 64L,
								riesz_batch_size = 64L,
								device = c("cpu", "cuda", "mps")) {
	checkmate::assert_number(crossfit_folds)
	checkmate::assert_number(zprime_folds)
	checkmate::assert_number(noise_dim)
	checkmate::assert_number(hidden_dim)
	checkmate::assert_number(num_layer)
	checkmate::assert_number(num_epochs)
	checkmate::assert_number(lr)
	checkmate::assert_number(riesz_epochs)
	checkmate::assert_number(riesz_lr)
	checkmate::assert_number(batch_size)
	checkmate::assert_number(riesz_batch_size)
	checkmate::assert_character(match.arg(device), len = 1)
	list(
		crossfit_folds = crossfit_folds,
		zprime_folds = zprime_folds,
		noise_dim = noise_dim,
		hidden_dim = hidden_dim,
		num_layer = num_layer,
		num_epochs = num_epochs,
		lr = lr,
		batch_size = as.numeric(batch_size),
		riesz_epochs = riesz_epochs,
		riesz_lr = riesz_lr,
		riesz_batch_size = as.numeric(riesz_batch_size),
		device = torch::torch_device(match.arg(device))
	)
}
