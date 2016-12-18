#' Machine learning models
#'
#' Add a learning model to a pipelearner object.
#'
#' Will expect a pipelearner object, but will also accept a data frame for the
#' parameter \code{pl}. In the case that a data frame is used, it will first
#' coerce it to a pipelearner object via \code{\link{pipelearner}}.
#'
#' @inheritParams pipelearner_params
#' @param ... Function calls of the learning models to use
#' @export
learn_models <- function(pl, ...) {
  UseMethod("learn_models")
}

#' @export
learn_models.default <- function(pl, ...) {
  learn_models.pipelearner(pipelearner(pl), ...)
}

#' @export
learn_models.pipelearner <- function(pl, ...) {

  mod_names <- lazyeval::lazy_dots(...) %>% purrr::map("expr") %>% as.character()
  function_calls <- lazyeval::uqs(list(...))

  models <- tibble::tibble(
    model = mod_names,
    .f    = function_calls
  )

  # models <- tibble::tibble(
  #   model = deparse(...),
  #   .f    = list(...)
  # )
  #
  # # Check at least one model provided
  # if (!nrow(models)) stop("Please provide at least one learning model function")

  pl$models <- models
  pl
}
