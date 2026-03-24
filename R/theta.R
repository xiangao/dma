estimate_theta <- function(cd, folds, params, control) {
	cli::cli_progress_step("Fitting outcome regressions... {control$crossfit_folds} folds")

	thetas <- future.apply::future_lapply(
		seq_along(folds),
		function(i) {
			train <- training(cd, folds, i)
			valid <- validation(cd, folds, i)
			theta(train, valid, cd@vars, params, control)
		},
		future.seed = TRUE
	)

	cli::cli_progress_done()

	# Extract per-fold models before recombining
	models_y <- lapply(thetas, function(x) x$model_y)

	result <- recombine_theta(thetas, folds)
	result$models_y <- models_y
	result
}

# Fit engression and predict on multiple named datasets
fit_and_predict <- function(X_train, Y_train, pred_data_list, x_vars, ref_cols, control, weights = NULL) {
	model <- engression::engression(
		X = X_train,
		Y = matrix(Y_train, ncol = 1),
		noise_dim = control$noise_dim,
		hidden_dim = control$hidden_dim,
		num_layer = control$num_layer,
		num_epochs = control$num_epochs,
		lr = control$lr,
		batch_size = control$batch_size,
		weights = weights,
		silent = TRUE
	)
	preds <- lapply(pred_data_list, function(d) {
		as.numeric(predict(model, prepare_engression_x(d, x_vars, ref_cols), type = "mean"))
	})
	list(model = model, preds = preds)
}

theta <- function(train, valid, vars, params, control) {
	continuous <- !is_binary(train$data[[vars@Y]])
	valid_dfs <- valid[sapply(valid, \(x) is.data.frame(x) && ncol(x) > 0)]
	obs <- censored(train$data, vars@C)
	weights <- train$weights[obs]

	# --- Top-level: Y ~ (A, W, M, Z) ---
	x_vars <- na.omit(c(vars@A, vars@W, vars@M, vars@Z))
	X_train <- prepare_engression_x(train$data[obs, , drop = FALSE], x_vars)
	ref_cols <- colnames(X_train)
	Y_train <- as.numeric(train$data[obs, vars@Y])

	# Build prediction list: all shifted train + valid regimes (exclude empty dfs)
	train_shifted <- train[-1]  # drop $data
	train_shifted <- train_shifted[sapply(train_shifted, \(x) is.data.frame(x) && ncol(x) > 0)]
	pred_data <- c(valid_dfs, setNames(train_shifted, paste0("train_", names(train_shifted))))
	pred_data <- pred_data[sapply(pred_data, is.data.frame)]

	fit_y <- fit_and_predict(X_train, Y_train, pred_data, x_vars, ref_cols, control, weights = weights)

	# Natural path ---------------------------------------------------------------

	if (length(params$natural) != 0) {
		vals_n <- vector("list", length = length(params$natural))
		names(vals_n) <- unlist(lapply(params$natural, \(x) paste0(gsub("data_", "", x), collapse = "")))

		for (s in seq_along(params$natural)) {
			j <- params$natural[[s]]["j"]
			k <- params$natural[[s]]["k"]
			l <- params$natural[[s]]["l"]

			b3_train <- fit_y$preds[[paste0("train_", j)]]
			b3_valid <- fit_y$preds[[j]]

			# theta2: b3 ~ (A, W, Z)
			x2_vars <- na.omit(c(vars@A, vars@W, vars@Z))
			X2 <- prepare_engression_x(train$data, x2_vars)
			ref2 <- colnames(X2)
			pred2 <- list(k_train = train[[k]], k_valid = valid_dfs[[k]], data_valid = valid_dfs$data)

			fit2 <- fit_and_predict(X2, b3_train, pred2, x2_vars, ref2, control, weights = train$weights)
			b2_train <- fit2$preds$k_train
			b2_valid <- fit2$preds$k_valid

			# theta1: b2 ~ (A, W)
			x1_vars <- c(vars@A, vars@W)
			X1 <- prepare_engression_x(train$data, x1_vars)
			ref1 <- colnames(X1)
			pred1 <- list(data_valid = valid_dfs$data, l_valid = valid_dfs[[l]])

			fit1 <- fit_and_predict(X1, b2_train, pred1, x1_vars, ref1, control, weights = train$weights)

			vals_n[[s]] <- list(
				fit3_natural = fit_y$preds$data,
				b3 = b3_valid,
				fit2_natural = fit2$preds$data_valid,
				b2 = b2_valid,
				fit1_natural = fit1$preds$data_valid,
				b1 = fit1$preds$l_valid
			)
		}

		if (length(params$randomized) == 0) {
			return(list(n = vals_n, model_y = fit_y$model))
		}
	}

	# Randomized path ------------------------------------------------------------

	vals_r <- vector("list", length = length(params$randomized))
	for (s in seq_along(params$randomized)) {
		i <- params$randomized[[s]]["i"]
		j <- params$randomized[[s]]["j"]
		k <- params$randomized[[s]]["k"]
		l <- params$randomized[[s]]["l"]

		b4_train <- fit_y$preds[[paste0("train_", i)]]
		b4_valid <- fit_y$preds[[i]]

		# theta3: b4 ~ (A, W, M)
		x3_vars <- c(vars@A, vars@W, vars@M)
		X3 <- prepare_engression_x(train$data, x3_vars)
		ref3 <- colnames(X3)
		pred3 <- list(j_train = train[[j]], j_valid = valid_dfs[[j]], data_valid = valid_dfs$data)

		fit3 <- fit_and_predict(X3, b4_train, pred3, x3_vars, ref3, control, weights = train$weights)
		b3_train <- fit3$preds$j_train
		b3_valid <- fit3$preds$j_valid

		# theta2: b3 ~ (A, W, Z)
		x2_vars <- na.omit(c(vars@A, vars@W, vars@Z))
		X2 <- prepare_engression_x(train$data, x2_vars)
		ref2 <- colnames(X2)
		pred2 <- list(k_train = train[[k]], k_valid = valid_dfs[[k]], data_valid = valid_dfs$data)

		fit2 <- fit_and_predict(X2, b3_train, pred2, x2_vars, ref2, control, weights = train$weights)
		b2_train <- fit2$preds$k_train
		b2_valid <- fit2$preds$k_valid

		# theta1: b2 ~ (A, W)
		x1_vars <- c(vars@A, vars@W)
		X1 <- prepare_engression_x(train$data, x1_vars)
		ref1 <- colnames(X1)
		pred1 <- list(data_valid = valid_dfs$data, l_valid = valid_dfs[[l]])

		fit1 <- fit_and_predict(X1, b2_train, pred1, x1_vars, ref1, control, weights = train$weights)

		vals_r[[s]] <- list(
			fit4_natural = fit_y$preds$data,
			b4 = b4_valid,
			fit3_natural = fit3$preds$data_valid,
			b3 = b3_valid,
			fit2_natural = fit2$preds$data_valid,
			b2 = b2_valid,
			fit1_natural = fit1$preds$data_valid,
			b1 = fit1$preds$l_valid
		)
	}

	names(vals_r) <-
		gsub("zp", "", unlist(lapply(params$randomized, \(x) paste0(gsub("data_", "", x), collapse = ""))))

	if (length(params$natural) == 0) {
		return(list(r = vals_r, model_y = fit_y$model))
	}

	list(n = vals_n, r = vals_r, model_y = fit_y$model)
}
