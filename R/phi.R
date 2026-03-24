estimate_phi_n_alpha <- function(cd, folds, params, nn_module, control, ref_levels = NULL) {
	if (length(params$natural) != 0) {
		cli::cli_progress_step("Computing alpha n density ratios... {control$crossfit_folds} folds")

		alpha_ns <- future.apply::future_lapply(
			seq_along(folds),
			function(i) {
				train <- training(cd, folds, i)
				valid <- validation(cd, folds, i)

				res <- lapply(
					params$natural,
					\(param) phi_n_alpha(train, valid, cd@vars, nn_module, param, control, ref_levels)
				)
				names(res) <- unlist(lapply(params$natural, \(x) paste0(gsub("data_", "", x), collapse = "")))
				res
			},
			future.seed = TRUE
		)

		cli::cli_progress_done()

		return(recombine_alpha(alpha_ns, folds))
	}
	NULL
}

phi_n_alpha <- function(train, valid, vars, architecture, params, control, ref_levels = NULL) {
	j <- params[1]
	k <- params[2]
	l <- params[3]

	.f1 <- \(alpha, dl) alpha(dl[[l]])
	.f2 <- \(alpha, dl) alpha(dl[[k]])
	.f3 <- \(alpha, dl) alpha(dl[[j]])

	alpha1 <- Alpha(
		train = train,
		valid = valid,
		vars = na.omit(c(vars@A, vars@W)),
		architecture = architecture,
		.f = .f1,
		weights = train$weights,
		control = control,
		ref_levels = ref_levels
	)

	alpha2 <- Alpha(
		train = train,
		valid = valid,
		vars = na.omit(c(vars@A, vars@Z, vars@W)),
		architecture = architecture,
		.f = .f2,
		weights = alpha1$train * train$weights,
		control = control,
		ref_levels = ref_levels
	)

	alpha3 <- Alpha(
		train = train,
		valid = valid,
		vars = na.omit(c(vars@A, vars@C, vars@M, vars@Z, vars@W)),
		architecture = architecture,
		.f = .f3,
		weights = alpha2$train * train$weights,
		control = control,
		ref_levels = ref_levels
	)

	list(jkl = gsub("data_", "", paste0(j, k, l, collapse = "")),
		 alpha1 = alpha1$valid,
		 alpha2 = alpha2$valid,
		 alpha3 = alpha3$valid)
}

estimate_phi_r_alpha <- function(cd, folds, params, nn_module, control, ref_levels = NULL) {
	if (length(params$randomized) != 0) {
		cli::cli_progress_step("Computing alpha r density ratios... {control$crossfit_folds} folds")

		alpha_rs <- future.apply::future_lapply(
			seq_along(folds),
			function(i) {
				train <- training(cd, folds, i)
				valid <- validation(cd, folds, i)

				res <- lapply(
					params$randomized,
					\(param) phi_r_alpha(train, valid, cd@vars, nn_module, param, control, ref_levels)
				)
				names(res) <- gsub("zp", "", unlist(lapply(
					params$randomized, \(x) paste0(gsub("data_", "", x), collapse = "")
				)))
				res
			},
			future.seed = TRUE
		)

		cli::cli_progress_done()

		return(recombine_alpha(alpha_rs, folds))
	}
	NULL
}

phi_r_alpha <- function(train, valid, vars, architecture, params, control, ref_levels = NULL) {
	i <- params[1]
	j <- params[2]
	k <- params[3]
	l <- params[4]

	.f1 <- \(alpha, data) alpha(data[[l]])
	.f2 <- \(alpha, data) alpha(data[[k]])
	.f3 <- \(alpha, data) alpha(data[[j]])
	.f4 <- \(alpha, data) alpha(data[[i]])

	alpha1 <- Alpha(
		train = train,
		valid = valid,
		vars = na.omit(c(vars@A, vars@W)),
		architecture = architecture,
		.f = .f1,
		weights = train$weights,
		control = control,
		ref_levels = ref_levels
	)

	alpha2 <- Alpha(
		train = train,
		valid = valid,
		vars = na.omit(c(vars@A, vars@Z, vars@W)),
		architecture = architecture,
		.f = .f2,
		weights = alpha1$train * train$weights,
		control = control,
		ref_levels = ref_levels
	)

	alpha3 <- Alpha(
		train = train,
		valid = valid,
		vars = na.omit(c(vars@A, vars@M, vars@W)),
		architecture = architecture,
		.f = .f3,
		weights = alpha2$train * train$weights,
		control = control,
		ref_levels = ref_levels
	)

	alpha4 <- Alpha(
		train = train,
		valid = valid,
		vars = na.omit(c(vars@A, vars@C, vars@Z, vars@M, vars@W)),
		architecture = architecture,
		.f = .f4,
		weights = alpha3$train * train$weights,
		control = control,
		ref_levels = ref_levels
	)

	list(ijkl = gsub("data_", "", paste0(i, j, k, l, collapse = "")),
		 alpha1 = alpha1$valid,
		 alpha2 = alpha2$valid,
		 alpha3 = alpha3$valid,
		 alpha4 = alpha4$valid)
}
