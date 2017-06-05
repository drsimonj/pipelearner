#' Cross-validation pairs
#'
#' Add cross validation pairs to a pipelearner object. By default will create
#' a random partition of the data. Can use any function which returns a data
#' data frame with three columns: \code{train}, \code{test}, and \code{.id}.
#' \code{test} and \code{train} are list-columns with each entry of type
#' \code{resample}. \code{.id} is an atomic vector. If no function is specified,
#' random partitions will try to be created unless \code{k} is not NULL, in
#' which case k folds will be created. See \code{\link[modelr]{crossv_mc}} for
#' more details.
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
  learn_cvpairs.pipelearner(pipelearner(pl), ...)
}

#' @export
learn_cvpairs.pipelearner <- function(pl, .f, ...) {
  if (missing(.f)) {
    args <- list(...)
    if (!exists("k", where = args) || is.null(args$k)) {
      .f <- function(data, n = 1, test = 0.2) {
        crossv_mc(data, n = n, test = test)
      }
      cv <- .f(pl$data, ...)
    } else {
      .f <- function(data) crossv_kfold(data, k = args$k)
      cv <- .f(pl$data)
    }
  } else {
    assertthat::assert_that(is.function(.f))
    cv <- .f(data = pl$data, ...)
  }

  # Test object returned by .f
  assertthat::assert_that(all(c("test", "train", ".id") %in% names(cv)))
  all_resample <- function(x) {
    purrr::map(x, ~ inherits(., "resample")) %>% unlist() %>% all()
  }
  assertthat::assert_that(all_resample(cv$test),
                          msg = paste(".f does not return list-column test",
                                "with all entries of type \'resample\'")
                          )
  assertthat::assert_that(all_resample(cv$train),
                          msg = paste(".f does not return list-column train",
                                      "with all entries of type \'resample\'")
                          )
  assertthat::assert_that(is.atomic(cv$.id))

  # If assertions are passed, save cv as cv_pairs
  pl$cv_pairs <- cv
  pl
}


