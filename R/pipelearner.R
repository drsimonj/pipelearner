#' Create a new pipe learner
#'
#' \code{pipelearner} initializes a new pipelearner object. It is used to
#' specify the input data frame to subsequent machine learning algorithms.
#'
#' @param data Data frame containing all variables needed by learning algorithm
#'   formulae.
#' @export
pipelearner <- function(data = NULL) {
  UseMethod("pipelearner")
}

#' @export
pipelearner.default <- function(data = NULL) {
  pipelearner.data.frame(as.data.frame(data))
}

#' @export
pipelearner.data.frame <- function(data) {
  pl <- structure(list(
    data = data
  ), class = c("pipelearner"))

  pl
}

#' Reports whether x is a pipelearner object
#' @param x An object to test
#' @keywords internal
#' @export
is.pipelearner <- function(x) inherits(x, "pipelearner")
