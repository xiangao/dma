make_dataset <- function(data, x, device, ref_levels = NULL) {
	self <- NULL
	dataset <- torch::dataset(
		name = "tmp_dma_dataset",
		initialize = function(data, x, device, ref_levels) {
			n <- nrow(data[["data"]])
			self$idx <- torch::torch_tensor(seq_len(n), dtype = torch::torch_long(), device = device)
			for (df in names(data)) {
				if (is.data.frame(data[[df]]) && ncol(data[[df]]) > 0) {
					df_x <- data[[df]][, x, drop = FALSE]
					self[[df]] <- one_hot_encode(df_x, names(df_x), ref_levels = ref_levels) |>
						as_torch(device = device)
				}
			}
		},
		.getitem = function(i) {
			fields <- grep("data", names(self), value = TRUE)
			out <- setNames(lapply(fields, function(x) self[[x]][i, ]), fields)
			out$idx <- self$idx[i]
			out
		},
		.length = function() {
			self$data$size()[1]
		}
	)
	dataset(data, x, device, ref_levels)
}
