#' Cross-validation pairs
#'
#' Customise cross validation pairs of training and test data for pipelearner
#' object. By default, a single cross validation pair will be created with a
#' random 80% of the data being assigned for training and the other 20% of
#' testing. \code{learn_cvpairs} allows this to be customised through the use of
#' a function that takes a \code{data.frame} as input and returns a
#' \code{data.frame} with three columns: \code{train}, \code{test}, and
#' \code{.id}. \code{train} and \code{test} must be list-columns of
#' \code{\link[modelr]{resample}} objects, and \code{.id} is an atomic vector of
#' unique values.
#'
#' Will expect a pipelearner object, but will also accept a data frame for the
#' parameter \code{pl}. In the case that a data frame is used, it will first
#' coerce it to a pipelearner object via \code{\link{pipelearner}}.
#'
#' @seealso Example functions that can be used for argument \code{.f}:
#'   \code{\link[modelr]{crossv_mc}}, \code{\link[modelr]{crossv_kfold}}
#'
#' @inheritParams pipelearner_params
#' @param .f function like \code{\link[modelr]{crossv_mc}} that partitions a
#'   data.frame into test-training splits. \code{.f} must have an argument
#'   \code{data}, which takes a \code{data.frame}.  \code{.f} must return a
#'   \code{data.frame} with two list-columns of resample objects (\code{"train"}
#'   and \code{"test"}) and a column of unique atomic values (\code{".id"}). of
#'   unique values.
#' @param ... additional parameters to be passed to \code{.f}
#'
#' @examples
#' # Five-fold cross validation
#' pl <- pipelearner(mtcars)
#' learn_cvpairs(pl, crossv_kfold, k = 5)
#'
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
  assertthat::assert_that(all(assertthat::has_name(cv, c("train", "test", ".id"))))
  assertthat::assert_that(all(purrr::map_lgl(cv$train, inherits, "resample")))
  assertthat::assert_that(all(purrr::map_lgl(cv$test, inherits, "resample")))
  assertthat::assert_that(is.atomic(cv$.id))
  assertthat::assert_that(dplyr::n_distinct(cv$.id) == nrow(cv))

  # If assertions are passed, attach cv to pl
  pl$cv_pairs <- cv
  pl
}


