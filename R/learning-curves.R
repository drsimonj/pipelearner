#' Learning curves
#'
#' Add learning curves (instructions to train on different proportions of each
#' training set) to a pipelearner object.
#'
#' Will expect a pipelearner object, but will also accept a data frame for the
#' parameter \code{pl}. In the case that a data frame is used, it will first
#' coerce it to a pipelearner object via \code{\link{pipelearner}}.
#'
#' @inheritParams pipelearner_params
#' @param ... Numbers that are greater than zero and less than or equal to one
#'   representing the proportions of training data to learn on. Will default to
#'   1 if no values are provided.
#' @export
learn_curves <- function(pl, ...) {
  UseMethod("learn_curves")
}

#' @export
learn_curves.default <-  function(pl, ...) {
  pipelearner(pl, ...)
}

#' @export
learn_curves.pipelearner <- function(pl, ...) {
  ps <- c(...)

  # Set default value as 1
  if (!length(ps)) ps <- 1

  # Order values from smallest to largest
  ps <- ps[order(ps)]

  if (!is.numeric(ps) || !all(ps > 0 & ps <= 1))
    stop("ps must be a vector of numbers greater than 0 and less than or equal to 1")

  pl$train_ps <- ps
  pl
}
