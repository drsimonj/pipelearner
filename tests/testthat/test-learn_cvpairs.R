context("learn_cvpairs")

df <- data.frame(a = letters, b = 1:26)

test_that("default setup", {
  pl1 <- pipelearner(df)
  pl2 <- pipelearner(df) %>% learn_cvpairs()

  expect_equal(dim(pl1$cv_pairs), dim(pl2$cv_pairs))
  expect_equal(nrow(pl1$cv_pairs$train[[1]]), nrow(pl2$cv_pairs$train[[1]]))
})

test_that("uses right modelr call", {
  pl <- pipelearner(df)

  expect_equal(learn_cvpairs(pl, n = 4)$cv_pairs %>% dim(),
               modelr::crossv_mc(df, n = 4) %>% dim)

  expect_equal(learn_cvpairs(pl, k = 3)$cv_pairs %>% dim(),
               modelr::crossv_kfold(df, k = 3) %>% dim)

  # k takes precedence
  expect_equal(learn_cvpairs(pl, k = 3, n = 4)$cv_pairs %>% dim(),
               modelr::crossv_kfold(df, k = 3) %>% dim)

})

test_that("Coerces pipelearner", {
  lc <- learn_cvpairs(df, n = 4)
  pl <- pipelearner(df) %>% learn_cvpairs(n = 4)

  expect_equal(names(lc), names(pl))
  expect_equal(dim(lc$cv_pairs), dim(pl$cv_pairs))
})
