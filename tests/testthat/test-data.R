test_that("dma_data stores ref_levels from factors", {
  df <- data.frame(
    sports = factor(c("1", "2", "1", "2"), levels = c("1", "2")),
    bmi = c(20, 25, 22, 28),
    age = c(10, 15, 12, 14),
    exercises = c(1, 0, 1, 0)
  )

  cd <- dma:::dma_data(
    data = df,
    vars = dma:::dma_vars(
      A = "sports", Y = "bmi", M = "exercises",
      Z = NA_character_, W = "age",
      C = NA_character_, id = NA_character_
    ),
    weights = rep(1, 4),
    d0 = function(data, trt) factor(rep("1", nrow(data)), levels = c("1", "2")),
    d1 = function(data, trt) factor(rep("2", nrow(data)), levels = c("1", "2"))
  )

  expect_true("sports" %in% names(cd@ref_levels))
  expect_equal(cd@ref_levels$sports, c("1", "2"))
})

test_that("dma_vars stores variable names correctly", {
  vars <- dma:::dma_vars(
    A = "trt", Y = "outcome", M = c("m1", "m2"),
    Z = NA_character_, W = c("w1", "w2"),
    C = NA_character_, id = NA_character_
  )

  expect_equal(vars@A, "trt")
  expect_equal(vars@Y, "outcome")
  expect_equal(vars@M, c("m1", "m2"))
  expect_true(is.na(vars@Z))
})

test_that("dma_control has correct defaults", {
  ctrl <- dma:::dma_control()

  expect_equal(ctrl$crossfit_folds, 10L)
  expect_equal(ctrl$noise_dim, 5L)
  expect_equal(ctrl$hidden_dim, 100L)
  expect_equal(ctrl$num_layer, 3L)
  expect_equal(ctrl$num_epochs, 500L)
  expect_equal(ctrl$riesz_epochs, 100L)
  expect_equal(ctrl$riesz_batch_size, 64L)
  expect_equal(ctrl$device$type, "cpu")
})
