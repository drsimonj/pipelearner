#' Machine learning models
#'
#' Add a learning model to a pipelearner object.
#'
#' Will expect a pipelearner object, but will also accept a data frame for the
#' parameter \code{pl}. In the case that a data frame is used, it will first
#' coerce it to a pipelearner object via \code{\link{pipelearner}}.
#'
#' @inheritParams pipelearner_params
#' @param model Function call to the learning model
#' @param formulas List of objects of class "formula" (or one that can be
#'   coerced to that class): a symbolic description of the model to be fitted.
#' @param ... Additional arguments to be passed to the model function as
#'   hyperparameters
#' @export
learn_model <- function(pl, model, formulas, ...) {
  UseMethod("learn_model")
}

#' @export
learn_model.default <- function(pl, model, formulas, ...) {
  learn_model.pipelearner(pipelearner(pl), model = model, formulas = formulas, ...)
}

#' @export
learn_model.pipelearner <- function(pl, model, formulas, ...) {

  mod_name <- lazyeval::expr_text(model)
  # mod_func <- lazyeval::uqs(c(model))

  grid <- list(formula = formulas, ...) %>% purrr::cross_d()


#
#   mod_names <- lazyeval::lazy_dots(...) %>% purrr::map("expr") %>% as.character()
#   function_calls <- lazyeval::uqs(list(...))
#
#   models <- tibble::tibble(
#     model = mod_names,
#     .f    = function_calls
#   )

  # models <- tibble::tibble(
  #   model = deparse(...),
  #   .f    = list(...)
  # )
  #
  # # Check at least one model provided
  # if (!nrow(models)) stop("Please provide at least one learning model function")

  pl$models[[mod_name]] <- grid
  pl
}
