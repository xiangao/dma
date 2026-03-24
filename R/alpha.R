Alpha <- function(train, valid, vars, architecture, .f, weights = NULL, control, ref_levels = NULL) {
	model <- nn_riesz_representer(
		train = train,
		vars = vars,
		architecture = architecture,
		.f = .f,
		weights = weights,
		batch_size = control$riesz_batch_size,
		learning_rate = control$riesz_lr,
		epochs = control$riesz_epochs,
		device = control$device,
		ref_levels = ref_levels
	)

	list(
		train = as.numeric(
			model(
				as_torch(
					one_hot_encode(train[["data"]][, vars, drop = FALSE], vars, ref_levels = ref_levels),
					device = control$device
				)
			)
		),
		valid = as.numeric(
			model(
				as_torch(
					one_hot_encode(valid[["data"]][, vars, drop = FALSE], vars, ref_levels = ref_levels),
					device = control$device
				)
			)
		)
	)
}
