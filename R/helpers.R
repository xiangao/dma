add_pseudo <- function(data, x) {
	cbind("tmp_dma_pseudo_y" = x, data)
}

make_folds <- function(data, V, id, strata) {
	if (missing(strata)) {
		if (is.na(id)) {
			folds <- origami::make_folds(data, V = V)
		} else {
			folds <- origami::make_folds(data, cluster_ids = data[[id]], V = V)
		}

		if (V > 1) {
			return(folds)
		}

		folds[[1]]$training_set <- folds[[1]]$validation_set
		return(folds)
	}

	binomial <- is_binary(data[[strata]])
	if ((is.na(id) | length(unique(data[[id]])) == nrow(data)) & binomial) {
		strata <- data[[strata]]
		strata[is.na(strata)] <- 2
		folds <- origami::make_folds(data, V = V, strata_ids = strata)
	} else {
		if (is.na(id)) folds <- origami::make_folds(data, V = V)
		else folds <- origami::make_folds(data, cluster_ids = data[[id]], V = V)
	}

	if (V > 1) {
		return(folds)
	}

	folds[[1]]$training_set <- folds[[1]]$validation_set
	folds
}

as_torch <- function(data, device) {
	torch::torch_tensor(as.matrix(data), dtype = torch::torch_float(), device = device)
}

censored <- function(data, cens) {
	if (is.na(cens)) {
		return(rep(TRUE, nrow(data)))
	}
	return(data[[cens]] == 1)
}

is_binary <- function(x) {
	non_na_values <- na.omit(x)
	unique_values <- unique(non_na_values)
	if (!all(non_na_values %in% c(0, 1))) {
		return(FALSE)
	}
	TRUE
}

reduce_bind_reorder <- function(x, .f, name1, name2, i) {
	vals <- lapply(x, \(y) y[[name1]]) |>
		lapply(\(z) z[[name2]]) |>
		Reduce(.f, x = _)
	vals[order(i)]
}

recombine_theta <- function(x, folds) {
	ind <- Reduce(c, lapply(folds, function(x) x[["validation_set"]]))
	.f <- function(x) {
		ijkl <- names(x[[1]])

		b_names <- grep("^b", names(x[[1]][[1]]), value = TRUE)
		bs <- lapply(b_names, \(param) sapply(ijkl, \(ijkl) reduce_bind_reorder(x, c, ijkl, param, ind)))
		names(bs) <- b_names

		natural_names <- grep("_natural$", names(x[[1]][[1]]), value = TRUE)
		natural <- lapply(natural_names, \(param) sapply(ijkl, \(ijkl) reduce_bind_reorder(x, c, ijkl, param, ind)))
		names(natural) <- natural_names

		list(bs = bs, natural = natural)
	}

	theta_n <- if (!is.null(x[[1]]$n)) .f(lapply(x, \(x) x$n)) else NULL
	theta_r <- if (!is.null(x[[1]]$r)) .f(lapply(x, \(x) x$r)) else NULL
	list(theta_n = theta_n, theta_r = theta_r)
}

recombine_alpha <- function(x, folds) {
	ind <- Reduce(c, lapply(folds, function(x) x[["validation_set"]]))
	ijkl <- names(x[[1]])
	alpha_names <- grep("^alpha", names(x[[1]][[1]]), value = TRUE)
	setNames(
		lapply(alpha_names,
			\(param) sapply(ijkl,
				\(ijkl) reduce_bind_reorder(x, c, ijkl, param, ind))),
		alpha_names
	)
}

one_hot_encode <- function(data, vars, ref_levels = NULL) {
	if (length(vars) == 0 || all(is.na(vars))) {
		return(data.frame(row.names = seq_len(nrow(data))))
	}
	tmp <- data[, vars, drop = FALSE]
	# Apply reference factor levels to ensure consistent columns across folds
	if (!is.null(ref_levels)) {
		for (v in names(ref_levels)) {
			if (v %in% names(tmp)) {
				tmp[[v]] <- factor(tmp[[v]], levels = ref_levels[[v]])
			}
		}
	}
	as.data.frame(model.matrix(~ ., data = tmp))[, -1, drop = FALSE]
}

# Prepare data for engression: one-hot encode factors/characters, return numeric matrix
# ref_cols: if provided, align output columns to match reference (for prediction)
prepare_engression_x <- function(data, vars, ref_cols = NULL) {
	tmp <- data[, vars, drop = FALSE]
	# One-hot encode any factor/character columns
	has_factor <- sapply(tmp, \(x) is.factor(x) | is.character(x))
	if (any(has_factor)) {
		ohe <- as.data.frame(model.matrix(~ ., data = tmp))[, -1, drop = FALSE]
	} else {
		ohe <- tmp
	}
	mat <- as.matrix(ohe)

	if (!is.null(ref_cols)) {
		# Align to reference columns (add missing as 0, keep order)
		result <- matrix(0, nrow = nrow(mat), ncol = length(ref_cols))
		colnames(result) <- ref_cols
		shared <- intersect(colnames(mat), ref_cols)
		result[, shared] <- mat[, shared]
		return(result)
	}

	# For training: drop constant columns
	keep <- apply(mat, 2, \(x) length(unique(x)) > 1)
	mat[, keep, drop = FALSE]
}

no_Z <- function(vars) any(is.na(vars@Z))

is_normalized <- function(x, tolerance = .Machine$double.eps^0.5) {
	abs(mean(x) - 1) < tolerance
}

normalize <- function(x) {
	if (is_normalized(x)) return(x)
	x / mean(x)
}

# https://stackoverflow.com/questions/15263146/revert-list-structure
revert_list <- function(ls) { # @Josh O'Brien
	x <- lapply(ls, `[`, names(ls[[1]]))
	apply(do.call(rbind, x), 2, as.list)
}
