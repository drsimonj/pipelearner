#' Cross-validation pairs
#'
#' Add cross validation pairs to a pipelearner object. By default will create
#' a random partition of the data. Can use any function which returns a data
#' data frame with three columns: \code{train}, \code{test}, and \code{.id}.
#' \code{test} and \code{train} are list-columns with each entry of type
#' \code{resample}. \code{.id} is an atomic vector. If no function is specified,
#' will create a random partition with proportion 0.2 reserved for testing data.
#' See \code{\link[modelr]{crossv_mc}} for more details.
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
#' learn_cvpairs(pl, crossv_kfold, k = 4)
#' @export
learn_cvpairs <- function(pl, .f, ...) {
  UseMethod("learn_cvpairs")
}

#' @export
learn_cvpairs.default <-  function(pl, .f, ...) {
  learn_cvpairs.pipelearner(pipelearner(pl), .f, ...)
}

#' @export
learn_cvpairs.pipelearner <- function(pl, .f, ...) {
  if (missing(.f)) {
    pl$cv_pairs <- crossv_mc(pl$data, n = 1L, test = 0.2, id = ".id")
    return (pl)
  }

  assertthat::assert_that(is.function(.f))
  cv <- .f(data = pl$data, ...)

  # Test object returned by .f
  assertthat::assert_that(all(c("test", "train", ".id") %in% names(cv)))
  assertthat::assert_that(all(purrr::map_lgl(cv$train, inherits, "resample")))
  assertthat::assert_that(all(purrr::map_lgl(cv$test, inherits, "resample")))
  assertthat::assert_that(is.atomic(cv$.id))

  # If assertions are passed, attach cv to pl
  pl$cv_pairs <- cv
  pl
}


