context("learn_curves")

df <- data.frame(a = letters, b = 1:26)

test_that("default setup", {
  pl1 <- pipelearner(df)
  pl2 <- pipelearner(df) %>% learn_curves()

  expect_equal(pl1$train_ps, pl2$train_ps)
})

test_that("Errors when ! 0 < x <= 1", {
  expect_error(pipelearner(df) %>% learn_curves(0),
               "Only non-zero proprtions are allowed")

  expect_error(pipelearner(df) %>% learn_curves(1.1),
               "Only non-zero proprtions are allowed")

  expect_error(pipelearner(df) %>% learn_curves(-1),
               "Only non-zero proprtions are allowed")

  expect_error(pipelearner(df) %>% learn_curves("a"),
               "Only non-zero proprtions are allowed")

  expect_error(pipelearner(df) %>% learn_curves(TRUE),
               "Only non-zero proprtions are allowed")
})

test_that("Creates ordered vector", {

  ps <- c(.5, 1, .2)

  pl <- pipelearner(df) %>% learn_curves(ps)

  expect_equal(length(pl$train_ps), length(ps))
  expect_equal(pl$train_ps, ps[order(ps)])
})

test_that("Coerces pipelearner", {
  lc <- learn_curves(df, .1, .2)
  pl <- pipelearner(df) %>% learn_curves(.1, .2)

  expect_equal(names(lc), names(pl))
  expect_equal(lc$train_ps, pl$train_ps)
})
