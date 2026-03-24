d_id <- \(data, trt) data[[trt]]

test_that("dma runs end-to-end with natural effects (N)", {
  skip_if_not_installed("torch")
  if (!torch::torch_is_installed()) skip("torch not installed")

  set.seed(123)
  n <- 100
  W <- rnorm(n)
  A <- rbinom(n, 1, plogis(W))
  M <- rnorm(n, A + W)
  Y <- rnorm(n, A + M + W)
  data <- data.frame(A, M, Y, W)

  result <- dma::dma(
    data = data,
    trt = "A",
    outcome = "Y",
    mediators = "M",
    covar = "W",
    effect = "N",
    d0 = d_id,
    d1 = d_id,
    control = dma::dma_control(
      crossfit_folds = 1L,
      num_epochs = 1L,
      riesz_epochs = 1L,
      riesz_batch_size = 32L
    )
  )

  expect_s3_class(result, "dma_result")
  expect_true(!is.null(result$estimates))
  expect_equal(result$effect, "N")
})

test_that("dma runs end-to-end with RI effects", {
  skip_if_not_installed("torch")
  if (!torch::torch_is_installed()) skip("torch not installed")
  skip_if_not_installed("Rsymphony")

  set.seed(123)
  n <- 100
  W <- rnorm(n)
  A <- rbinom(n, 1, plogis(W))
  Z <- rnorm(n, A + W)
  M <- rnorm(n, A + Z + W)
  Y <- rnorm(n, A + M + Z + W)
  data <- data.frame(A, M, Z, Y, W)

  result <- dma::dma(
    data = data,
    trt = "A",
    outcome = "Y",
    mediators = "M",
    moc = "Z",
    covar = "W",
    effect = "RI",
    d0 = d_id,
    d1 = d_id,
    control = dma::dma_control(
      crossfit_folds = 1L,
      num_epochs = 1L,
      riesz_epochs = 1L,
      riesz_batch_size = 32L
    )
  )

  expect_s3_class(result, "dma_result")
  expect_equal(result$effect, "RI")
})

test_that("tidy returns a data.frame with correct columns", {
  skip_if_not_installed("torch")
  if (!torch::torch_is_installed()) skip("torch not installed")

  set.seed(123)
  n <- 100
  W <- rnorm(n)
  A <- rbinom(n, 1, plogis(W))
  M <- rnorm(n, A + W)
  Y <- rnorm(n, A + M + W)
  data <- data.frame(A, M, Y, W)

  result <- dma::dma(
    data = data, trt = "A", outcome = "Y", mediators = "M", covar = "W",
    effect = "N", d0 = d_id, d1 = d_id,
    control = dma::dma_control(crossfit_folds = 1L, num_epochs = 1L, riesz_epochs = 1L)
  )

  td <- dma:::tidy.dma_result(result)
  expect_true(is.data.frame(td))
  expect_true(all(c("term", "estimate", "std.error", "conf.low", "conf.high") %in% names(td)))
})

test_that("one-hot encoding is consistent across folds", {
  vars <- dma:::dma_vars(
    A = "A", Y = "Y", M = "M", W = "W",
    Z = NA_character_, C = NA_character_, id = NA_character_
  )
  cd <- dma:::dma_data(
    data = data.frame(
      A = factor(c("a", "b", "a", "b")),
      M = rnorm(4), Y = rnorm(4), W = rnorm(4)
    ),
    vars = vars,
    weights = rep(1, 4),
    d0 = d_id, d1 = d_id
  )

  expect_true("A" %in% names(cd@ref_levels))
  expect_equal(cd@ref_levels$A, c("a", "b"))

  ohe1 <- dma:::one_hot_encode(
    cd@data[1:2, ], "A", ref_levels = cd@ref_levels
  )
  ohe2 <- dma:::one_hot_encode(
    cd@data[3:4, ], "A", ref_levels = cd@ref_levels
  )
  expect_equal(names(ohe1), names(ohe2))
})
