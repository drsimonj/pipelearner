context("pipelearner")

df <- data.frame(
  a = letters,
  b = 1:26,
  stringsAsFactors = FALSE)

test_that("takes/coerces data frame/tibble", {
  expect_equal(pipelearner(df)$data, tibble::as_tibble(df))
  expect_equal(pipelearner(as.list(df))$data, tibble::as_tibble(df))
  expect_equal(pipelearner(c(1,2,3))$data, tibble::as_tibble(c(1,2,3)))
  expect_error(pipelearner())
})

test_that("is pipelearner?", {
  expect_is(pipelearner(df), "pipelearner")
  expect_true(pipelearner(df) %>% is.pipelearner())
  expect_false(is.pipelearner(1))
})

test_that("default setup", {
  pl <- pipelearner(df)

  expect_equal(nrow(pl$cv_pairs), 1)
  expect_equal(names(pl$cv_pairs), c("train", "test", ".id"))
  expect_equal(pl$train_ps, 1)
  expect_equal(pl$models, NULL)
  expect_equal(pl$fits, NULL)
})

test_that("model setup", {
  pl <- pipelearner(df, models = c(lm, glm), formulas = c(mpg ~ hp, am ~ wt))

  expect_equal(nrow(pl$models), 4)
  expect_equal(names(pl$models), c(".f", "params"))
})
