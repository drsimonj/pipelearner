context("learn_cvpairs")
library(resamplr)

df <- data.frame(a = letters, b = 1:26)

test_that("default setup", {
  pl1 <- pipelearner(df)
  pl2 <- pipelearner(df) %>% learn_cvpairs()

  expect_equal(dim(pl1$cv_pairs), dim(pl2$cv_pairs))
  expect_equal(nrow(pl1$cv_pairs$train[[1]]), nrow(pl2$cv_pairs$train[[1]]))
})

test_that("uses right modelr call", {
  pl <- pipelearner(df)

  pl_cv <- learn_cvpairs(pl)$cv_pairs
  modelr_cv <- modelr::crossv_mc(df, n = 1)

  expect_equal(dim(pl_cv), dim(modelr_cv))
  expect_equal(names(pl_cv), names(modelr_cv))
})

test_that("Coerces pipelearner", {
  lc <- learn_cvpairs(df)
  pl <- pipelearner(df) %>% learn_cvpairs()

  expect_equal(names(lc), names(pl))
  expect_equal(dim(lc$cv_pairs), dim(pl$cv_pairs))
})

test_that("Calls resamplr functions", {
  pl <- pipelearner(df)
  pl_cv <- learn_cvpairs(pl, crossv_ts, horizon = 2L, by = 3L)$cv_pairs
  ts_cv <- crossv_ts(df, horizon = 2L, by = 3L)

  expect_equal(dim(pl_cv), dim(ts_cv))
  expect_equal(names(pl_cv), names(ts_cv))
})
