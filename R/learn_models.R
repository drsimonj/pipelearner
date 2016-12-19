#' Machine learning models
#'
#' Add a learning model to a pipelearner object.
#'
#' Will expect a pipelearner object, but will also accept a data frame for the
#' parameter \code{pl}. In the case that a data frame is used, it will first
#' coerce it to a pipelearner object via \code{\link{pipelearner}}.
#'
#' @inheritParams pipelearner_params
#' @param models Vecor of learning model functions
#' @param formulas Vector of objects of class "formula" (or one that can be
#'   coerced to that class): a symbolic description of the model to be fitted.
#' @param ... Additional named vectors to be passed to the model function as a
#'   grid of hyperparameters
#' @export
learn_models <- function(pl, models, formulas, ...) {
  UseMethod("learn_models")
}

#' @export
learn_models.default <- function(pl, models, formulas, ...) {
  #model <- lazyeval::expr_text(model)
  pipelearner(pl) %>% learn_models(models = models, formulas = formulas, ...)
}

#' @export
learn_models.pipelearner <- function(pl, models, formulas, ...) {

  if (missing(models)) stop("'models' is missing with no default")
  if (missing(formulas)) stop("'formulas' is missing with no default")

  # formulas should be a vector
  formulas <- c(formulas)

  # Create complete parameter grid in single params column
  params <- list(formula = formulas, ...) %>%
            purrr::cross_d() %>%
            dplyr::mutate(.id = row_number()) %>%
            tidyr::nest(-.id, .key = params) %>%
            dplyr::mutate(params = purrr::map(params, unlist))

  models <- list(.f = c(models), params = params$params) %>%
    purrr::cross_d() %>%
    dplyr::mutate(.id = NA)

  pl$models <- rbind(pl$models, models) %>%
    dplyr::mutate(.id = seq(nrow(.)))

  pl
}
