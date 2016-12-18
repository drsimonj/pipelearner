context("learn_models")

df <- data.frame(a = letters, b = 1:26)

test_that("single call", {
  pl1 <- pipelearner(df) %>% learn_models(lm, mpg~hp)
  pl2 <- pipelearner(df) %>% learn_models(lm, c(mpg~hp, hp~am))
  pl8 <- pipelearner(df) %>% learn_models(c(lm, glm), c(mpg~hp, hp~am), method = c("a", "b"))

  expect_equal(names(pl1$models), c(".f", "params"))
  expect_equal(nrow(pl1$models), 1)
  expect_equal(nrow(pl2$models), 2)
  expect_equal(nrow(pl8$models), 8)

  expect_equal(nrow(pl1$models), nrow(pipelearner(df, lm, mpg~hp)$models))
})

test_that("piped calls", {
  pl2 <- pipelearner(df) %>%
    learn_models(lm, mpg~hp) %>%
    learn_models(glm, am~vs)

  pl4 <- pl2 %>% learn_models(rpart::rpart, mpg~hp, minsplit = c(1, 2))

  expect_equal(nrow(pl2$models), 2)
  expect_equal(nrow(pl4$models), 4)
})

test_that("requires model and formula", {
  pl <- pipelearner(df)

  expect_error(learn_models(pl),
               "'models' and/or 'formulas' are missing, with no default")

  expect_error(learn_models(pl, models = lm),
               "'models' and/or 'formulas' are missing, with no default")

  expect_error(learn_models(pl, formulas = mpg ~ hp),
               "'models' and/or 'formulas' are missing, with no default")
})

test_that("Coerces pipelearner", {
  lmod <- learn_models(df, lm, mpg ~ hp)
  pl <- pipelearner(df) %>% learn_models(lm, mpg ~ hp)

  expect_equal(names(lmod), names(pl))
  expect_equal(dim(lmod$models), dim(pl$models))
})
