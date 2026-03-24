test_that("one_hot_encode produces consistent columns with ref_levels", {
  df1 <- data.frame(x = factor(c("a", "b", "c")), y = 1:3)
  df2 <- data.frame(x = factor(c("a", "a", "b")), y = 4:6)

  ref_levels <- list(x = c("a", "b", "c"))

  ohe1 <- dma:::one_hot_encode(df1, c("x", "y"), ref_levels = ref_levels)
  ohe2 <- dma:::one_hot_encode(df2, c("x", "y"), ref_levels = ref_levels)

  expect_equal(ncol(ohe1), ncol(ohe2))
  expect_equal(names(ohe1), names(ohe2))
})

test_that("one_hot_encode works without ref_levels", {
  df <- data.frame(x = factor(c("a", "b", "c")), y = 1:3)
  ohe <- dma:::one_hot_encode(df, c("x", "y"))

  expect_true(is.data.frame(ohe))
  expect_true(ncol(ohe) > 0)
  expect_equal(nrow(ohe), 3)
})

test_that("prepare_engression_x drops constant columns for training", {
  df <- data.frame(x = c(1, 1, 1), y = c(1, 2, 3), z = c(4, 5, 6))
  result <- dma:::prepare_engression_x(df, c("x", "y", "z"))

  expect_false("x" %in% colnames(result))
  expect_true("y" %in% colnames(result))
  expect_true("z" %in% colnames(result))
})

test_that("prepare_engression_x aligns to ref_cols for prediction", {
  df <- data.frame(x = c(1, 1, 1), y = c(1, 2, 3))
  ref <- c("x", "y", "z")
  result <- dma:::prepare_engression_x(df, c("x", "y"), ref_cols = ref)

  expect_equal(colnames(result), ref)
  expect_equal(result[, "z"], c(0, 0, 0))
})

test_that("is_binary correctly identifies binary vectors", {
  expect_true(dma:::is_binary(c(0, 1, 0, 1)))
  expect_true(dma:::is_binary(c(0, 0, 0)))
  expect_false(dma:::is_binary(c(0, 1, 2)))
  expect_false(dma:::is_binary(c(0.5, 1)))
})

test_that("normalize produces mean-1 weights", {
  x <- c(2, 4, 6)
  result <- dma:::normalize(x)
  expect_equal(mean(result), 1)
})

test_that("shift_data modifies treatment column", {
  df <- data.frame(
    sports = factor(c("1", "2", "1"), levels = c("1", "2")),
    age = c(10, 20, 30)
  )
  d0 <- function(data, trt) factor(rep("1", nrow(data)), levels = c("1", "2"))

  result <- dma:::shift_data(df, "sports", NA_character_, d0)

  expect_true(all(result$sports == "1"))
  expect_equal(result$age, df$age)
})
