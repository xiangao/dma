#' @importFrom checkmate `%??%`
nn_riesz_representer <- function(train,
								 vars,
								 architecture,
								 .f,
								 weights = NULL,
								 batch_size,
								 learning_rate,
								 epochs,
								 device,
								 ref_levels = NULL) {
	dataset <- make_dataset(train, vars, device = device, ref_levels = ref_levels)
	train_dl <- torch::dataloader(dataset, batch_size = batch_size)
	model <- architecture(ncol(dataset$data))
	model$to(device = device)

	weights <- weights %??% 1
	if (length(weights) > 1) {
		weights <- torch::torch_tensor(weights, dtype = torch::torch_float(), device = device)
	}

	optimizer <- torch::optim_adam(
		params = c(model$parameters),
		lr = learning_rate,
		weight_decay = 0.01
	)

	scheduler <- torch::lr_one_cycle(
		optimizer,
		max_lr = learning_rate,
		total_steps = epochs
	)

	for (epoch in 1:epochs) {
		coro::loop(for (b in train_dl) {
			w <- if (is.numeric(weights)) weights else weights[b$idx]
			loss <- (model(b$data)$pow(2) - (2 * w * .f(model, b)))$mean(dtype = torch::torch_float())

			optimizer$zero_grad()
			loss$backward()

			optimizer$step()
		})
		scheduler$step()
	}

	model$eval()
	model
}
