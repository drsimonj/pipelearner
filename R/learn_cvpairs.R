#' Cross-validation pairs
#'
#' Add cross validation pairs to a pipelearner object. By default will create
#' a random partition of the data. Can use any function which creates
#' \code{train} and \code{test} columns of \code{resample} objects and column
#' of \code{id} integers. See \code{\link[modelr]{crossv_mc}} and
#' \code{\link[modelr]{resample}} for details.
#'
#' Will expect a pipelearner object, but will also accept a data frame for the
#' parameter \code{pl}. In the case that a data frame is used, it will first
#' coerce it to a pipelearner object via \code{\link{pipelearner}}.
#'
#' @inheritParams pipelearner_params
#' @param .f a function which takes a \code{data} argument
#' @param ... additional parameters to be passed to \code{.f}
#'
#' @examples
#' pl <- pipelearner(mtcars)
#' learn_cvpairs(pl, modelr::crossv_kfold, k = 4)
#' learn_cvpairs(pl, resamplr::holdout_frac, size = 0.3, K = 5L)
#' @export
learn_cvpairs <- function(pl, .f = function(data) modelr::crossv_mc(data, n = 1), ...) {
  UseMethod("learn_cvpairs")
}

#' @export
learn_cvpairs.default <-  function(pl, .f = function(data) modelr::crossv_mc(data, n = 1), ...) {
  learn_cvpairs.pipelearner(pipelearner(pl), ...)
}

#' @export
learn_cvpairs.pipelearner <- function(pl, .f = function(data) modelr::crossv_mc(data, n = 1), ...) {
  pl$cv_pairs <- purrr::invoke(.f, data = pl$data, ...)
  pl
}


