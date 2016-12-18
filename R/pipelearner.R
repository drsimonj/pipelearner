#' Create a new pipe learner
#'
#' \code{pipelearner} initializes a new pipelearner object. It is used to
#' specify the input data frame to subsequent machine learning algorithms.
#'
#' @param data Data frame containing all variables needed by learning algorithm
#'   formulae.
#' @inheritParams learn_model
#' @export
pipelearner <- function(data, model = NULL, formulas = NULL, ...) {
  UseMethod("pipelearner")
}

#' @export
pipelearner.default <- function(data, model = NULL, formulas = NULL, ...) {
  pipelearner.data.frame(as.data.frame(data), model = model, formulas = formulas, ...)
}

#' @export
pipelearner.data.frame <- function(data, model = NULL, formulas = NULL, ...) {

  pl <- structure(list(
    data     = data,
    cv_pairs = NULL,
    train_ps = NULL,
    models   = NULL
  ), class = c("pipelearner"))

  # Set defaults
  pl <- learn_cvpairs(pl)   # cross-validation pairs
  pl <- learn_curves(pl)    # learning curves

  if (!is.null(model))
    pl <- learn_model(pl, model = model, formulas = formulas, ...)

  pl
}

#' Reports whether x is a pipelearner object
#' @param x An object to test
#' @keywords internal
#' @export
is.pipelearner <- function(x) inherits(x, "pipelearner")


#' Parameters shared by pipelearner methods.
#'
#' @name pipelearner_params
#' @param pl pipelearner object. See \code{\link{pipelearner}}
NULL
