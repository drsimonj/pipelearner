#' Cross-validation pairs
#'
#' Add cross validation pairs to a pipelearner object. \code{n} random
#' partitions will try to be created unless \code{k} is not NULL, in which case
#' k folds will be created. See \code{\link[modelr]{crossv_mc}} for more
#' details.
#'
#' \code{learn_cvpairs} will expect a pipelearner object, but will also accept a
#' data frame for the parameter \code{pl}. In the case that a data frame is
#' used, it will first coerce it to a pipelearner object via
#' \code{\link{pipelearner}}.
#'
#' @inheritParams pipelearner_params
#' @inheritParams modelr::crossv_mc
#' @export
learn_cvpairs <- function(pl, k = NULL, n = 1, test = .02) {
  UseMethod("learn_cvpairs")
}

#' @export
learn_cvpairs.default <-  function(pl, k = NULL, n = 1, test = .02) {
  learn_cvpairs.pipelearner(pipelearner(pl), k = k, n = n, test = test)
}

#' @export
learn_cvpairs.pipelearner <- function(pl, k = NULL, n = 1, test = .02) {

  # n random partitions
  if (is.null(k)) {
    cv_pairs <- modelr::crossv_mc(pl$data, n = n, test = test, id = ".id")
  } else {
    cv_pairs <- modelr::crossv_kfold(pl$data, k = k, id = ".id")
  }

  pl$cv_pairs <- cv_pairs
  pl
}

