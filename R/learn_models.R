#' Machine learning models
#'
#' Add a learning model to a pipelearner object.
#'
#' Will expect a pipelearner object, but will also accept a data frame for the
#' parameter \code{pl}. In the case that a data frame is used, it will first
#' coerce it to a pipelearner object via \code{\link{pipelearner}}.
#'
#' @inheritParams pipelearner_params
#' @param models Vector of learning model functions which take data and formula
#'   arguments
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

  # Get model names (needs to happen first)
  model_names <- lazyeval::expr_find(models) %>% as.character()
  if (model_names[1] == "::")  model_names <- model_names[3]
  if (length(model_names) > 1) model_names <- model_names[-1]

  # Argument checks
  if (missing(models)) stop("'models' is missing with no default")
  if (missing(formulas)) stop("'formulas' is missing with no default")

  # models and formulas should be vectors
  models   <- c(models)
  formulas <- c(formulas)

  # Check all models are functions
  for (m in seq(models)) {
    if (!is.function(models[[m]]))
      stop("`", model_names[m], "` is not a function")

    # -- This checks if function provides formula and data arguments
    # -- However, it doesn't always work. E.g., randomForest::randomForest
    #if (!all(c("formula", "data") %in% names(formals(models[[m]]))))
    #  stop("`", model_names[m], "` does not have arguments 'formula' and 'data'")
  }

  # Create complete parameter grid in single params column
  params <- list(formula = formulas, ...) %>%
            purrr::cross_d() %>%
            dplyr::mutate(.id = row_number()) %>%
            tidyr::nest(-.id, .key = params) %>%
            dplyr::mutate(params = purrr::map(params, unlist))

  models <- list(.f = c(models), params = params$params) %>%
    purrr::cross_d() %>%
    dplyr::mutate(.id = NA,
                  target = purrr::map_chr(params, ~ lhs_as_chr(.$formula)))
  models$model <- model_names  # Recycled

  # Order columns
  models <- models %>%
    dplyr::select(target, model, params, .f, dplyr::everything(), .id)

  pl$models <- rbind(pl$models, models) %>%
    dplyr::mutate(.id = nrow(.) %>% seq_len() %>% as.character())

  pl
}

lhs_as_chr <- function(formula) {
  formula %>%
    lazyeval::f_lhs() %>%
    as.character()
}
