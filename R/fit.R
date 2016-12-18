#' Fit mahine learning models
#'
#' \code{pipelearner_fit} takes a pipelearner object and conducts the steps
#' needed to fit all of the specified models.
#'
#' @inheritParams pipelearner_params
#' @export
fit_learners <- function(pl) {

  if (is.null(pl$models))
    stop("There are no models to fit. Add models first with `learn_models`")

  # THIS IS A TEMPORARY TRIAL
  # TODO: fit to all cross validation pairs and training rates
  # pl$models <- pl$models %>%
  #   dplyr::mutate(fit = purrr::invoke_map(.$.f, .$params, data = pl$data))
  #
  fit_cvdata <- function(train_data, cv_id, models) {
    models %>%
      dplyr::mutate(fit = purrr::invoke_map(.$.f, .$params, data = train_data),
                    cv.id = cv_id)
  }

  pl$fits <- purrr::map2_df(pl$cv_pairs$train, pl$cv_pairs$.id, fit_cvdata, models = pl$models)

  pl
}
