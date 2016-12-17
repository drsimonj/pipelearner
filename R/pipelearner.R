#' Create a new pipe learner
#'
#' \code{pipelearner} initializes a new pipelearner object. It is used to
#' specify the input data frame to subsequent machine learning algorithms.
#'
#' @param data Data frame containing all variables needed by learning algorithm
#'   formulae.
#' @export
pipelearner <- function(data) {
  UseMethod("pipelearner")
}

#' @export
pipelearner.default <- function(data) {
  pipelearner.data.frame(as.data.frame(data))
}

#' @export
pipelearner.data.frame <- function(data) {
  pl <- structure(list(
    data = data,
    # Default .80 train and .20 test set
    cross_validation_pairs = crossv_mc(data, 1, test = 0.2)
  ), class = c("pipelearner"))

  pl
}

#' Reports whether x is a pipelearner object
#' @param x An object to test
#' @keywords internal
#' @export
is.pipelearner <- function(x) inherits(x, "pipelearner")
