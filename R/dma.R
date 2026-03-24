#' Causal mediation analysis using energy regression
#'
#' Estimates common mediation causal effects using engression for outcome
#' regressions and Riesz learning for density ratio estimation.
#' Supports natural effects, organic effects, randomized interventional effects,
#' and recanting twins decompositions.
#'
#' @param data [\code{data.frame}]\cr
#'  A \code{data.frame} containing all necessary variables.
#' @param trt [\code{character}]\cr
#'  Column names of treatment variables.
#' @param outcome [\code{character(1)}]\cr
#'  Column name of the outcome variable.
#' @param mediators [\code{character}]\cr
#'  Column names of mediator variables.
#' @param moc [\code{character}]\cr
#'  Optional column names of mediator-outcome confounders. Required for RI and RT effects.
#' @param covar [\code{character}]\cr
#'  Column names of baseline covariates.
#' @param obs [\code{character(1)}]\cr
#'  Optional column name for censoring indicator (0/1).
#' @param id [\code{character(1)}]\cr
#'  Optional column name for cluster identifiers.
#' @param d0 [\code{function}]\cr
#'  Shift function for control regime.
#' @param d1 [\code{function}]\cr
#'  Shift function for treatment regime.
#' @param effect [\code{character(1)}]\cr
#'  Effect type: "N" (natural), "O" (organic), "RI" (randomized interventional),
#'  or "RT" (recanting twins).
#' @param weights [\code{numeric}]\cr
#'  Optional survey weights.
#' @param nn_module [\code{function}]\cr
#'  A function returning a neural network module for Riesz estimation.
#' @param control [\code{list}]\cr
#'  Control parameters from \code{dma_control()}.
#'
#' @return An object of class \code{dma_result} with components:
#' \item{estimates}{Named list of effect estimates (ife objects).}
#' \item{outcome_reg}{Outcome regression predictions.}
#' \item{alpha_n}{Natural density ratio estimates.}
#' \item{alpha_r}{Randomized density ratio estimates.}
#' \item{models_y}{Trained engression models from cross-fitting.}
#' \item{folds}{Cross-fitting fold structure.}
#' \item{vars}{Variable specification (dma_vars object).}
#' \item{data}{Original data frame.}
#' \item{call}{The matched call.}
#' \item{effect}{The estimated effect type.}
#'
#' @importFrom checkmate assert_data_frame assert_function assert_numeric
#'
#' @export
#'
#' @examples
#' \donttest{
#' if (torch::torch_is_installed()) {
#'   # See vignette for full examples
#' }
#' }
dma <- function(data,
						trt,
						outcome,
						mediators,
						moc = NULL,
						covar,
						obs = NULL,
						id = NULL,
						d0 = NULL,
						d1 = NULL,
						effect = c("N", "O", "RI", "RT"),
						weights = rep(1, nrow(data)),
						nn_module = sequential_module(),
						control = dma_control()) {

	assert_data_frame(data[, c(trt, outcome, mediators, moc, covar, obs, id)])
	assert_not_missing(data, trt, covar, mediators, moc, obs)
	assert_function(d0, nargs = 2, null.ok = TRUE)
	assert_function(d1, nargs = 2, null.ok = TRUE)
	assert_function(nn_module)
	assert_binary_0_1(data, outcome)
	assert_binary_0_1(data, obs)
	assert_effect_type(moc, match.arg(effect))
	assert_numeric(weights, len = nrow(data), finite = TRUE, any.missing = FALSE)

	params <- switch(
		match.arg(effect),
		N = natural,
		O = organic,
		RT = recanting_twin,
		RI = randomized
	)

	cd <- dma_data(
		data = data,
		vars = dma_vars(
			A = trt,
			Y = outcome,
			M = mediators,
			Z = moc %??% NA_character_,
			W = covar,
			C = obs %??% NA_character_,
			id = id %??% NA_character_
		),
		weights = weights,
		d0 = d0,
		d1 = d1
	)

	cd <- add_zp(cd, moc, control)

	folds <- make_folds(cd@data, control$crossfit_folds, cd@vars@id, cd@vars@Y)

	thetas <- estimate_theta(cd, folds, params, control)

	alpha_ns <- estimate_phi_n_alpha(cd, folds, params, nn_module, control, cd@ref_levels)
	eif_ns <- calc_eifs(cd, alpha_ns, thetas, eif_n)

	alpha_rs <- estimate_phi_r_alpha(cd, folds, params, nn_module, control, cd@ref_levels)
	eif_rs <- calc_eifs(cd, alpha_rs, thetas, eif_r)

	new_dma_result(
		estimates = switch(
			match.arg(effect),
			N = calc_estimates_natural(eif_ns, weights),
			O = calc_estimates_organic(eif_ns, weights),
			RT = calc_estimates_rt(eif_ns, eif_rs, weights),
			RI = calc_estimates_ri(eif_rs, weights)
		),
		outcome_reg = thetas,
		alpha_n = alpha_ns %??% list(),
		alpha_r = alpha_rs %??% list(),
		models_y = thetas$models_y %??% list(),
		folds = folds,
		vars = cd@vars,
		data = data,
		call = match.call(),
		effect = match.arg(effect)
	)
}
