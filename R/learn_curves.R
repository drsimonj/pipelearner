#' Learning curves
#'
#' Add learning curves (instructions to train on different proportions of each
#' training set) to a pipelearner object.
#'
#' Will expect a pipelearner object, but will also accept a data frame for the
#' parameter \code{pl}. In the case that a data frame is used, it will first
#' coerce it to a pipelearner object via \code{\link{pipelearner}}.
#'
#' When these proportions are applied to cross-validation training sets, they
#' will always slice the proportion starting the beginning. For example, a
#' proportion of .5 will take the first 50\% of rows from the training data
#' (rounded to the nearest whole number). There is no point taking random
#' subsamples of size p from the training data because it would require us to
#' save these subsamples (taking up more space) despite the fact that the
#' training data is already a random sample (see \code{\link{learn_cvpairs}}).
#'
#' @inheritParams pipelearner_params
#' @param ... Non-zero proportions of training data to learn on. Will default to
#'   1 if no values are provided.
#' @export
learn_curves <- function(pl, ...) {
  UseMethod("learn_curves")
}

#' @export
learn_curves.default <-  function(pl, ...) {
  pipelearner(pl) %>% learn_curves(...)
}

#' @export
learn_curves.pipelearner <- function(pl, ...) {
  ps <- c(...)

  # Set default value as 1
  if (!length(ps)) ps <- 1

  # Order values from smallest to largest
  ps <- ps[order(ps)]

  if (!is.numeric(ps) || !all(ps > 0 & ps <= 1))
    stop("Only non-zero proprtions are allowed")

  pl$train_ps <- ps
  pl
}
