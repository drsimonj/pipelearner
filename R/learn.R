#' Learn from data
#'
#' \code{learn} conducts the steps needed to learn/fit all of the specified
#' models on all cross-validation training data sets, in all variations of
#' training sample size, in a pipelearner object, and return the results as a
#' summarized tibble.
#'
#' @inheritParams pipelearner_params
#' @export
#'
#' @examples
#' pl <- pipelearner(mtcars, lm, mpg ~ .)
#' learn(pl)
learn <- function(pl) {
  UseMethod("learn")
}

#' @export
learn.default <- function(pl) {
  stop("`learn` should only be used with a pipelearner object")
}

#' @export
learn.pipelearner <- function(pl) {

  if (is.null(pl$models))
    stop("No models to learn with. Add using `learn_models`")

  # Temporarily store all fitted models with list elements referenced by .id
  pl$fits <- fit_all(pl)

  # Return summarised tibble
  recover_fits(pl)
}


fit_all <- function(pl) {
  # Fit all models
  purrr::map_df(pl$train_ps, fit_p, pl$cv_pairs, pl$models) %>%
    # Arrange columns and rows
    dplyr::select(models.id, cv_pairs.id, train_p, dplyr::everything()) %>%
    dplyr::arrange(models.id, cv_pairs.id, train_p)
}

fit_cvdata <- function(train_data, cv_id, models) {
  tibble::tibble(
    cv_pairs.id = cv_id,
    models.id   = models$.id,
    fit         = purrr::invoke_map(models$.f,
                                    models$params,
                                    data = as.data.frame(train_data))
  )
}

fit_p <- function(p, cv_tbl, models) {
  # Fit models
  data_list <- purrr::map(cv_tbl$train, p_from_resample, p)

  to_fit <- list(train_data = data_list, cv_id = cv_tbl$.id)

  purrr::pmap_df(to_fit, fit_cvdata, models = models) %>%
    dplyr::mutate(train_p = p)

}

recover_fits <- function(pl) {
  pl$fits %>%
    # Recover relevant model and cv_pair info
    dplyr::mutate(
      models.row = purrr::map(models.id, ~ dplyr::filter(pl$models, .id == .) %>%
                                dplyr::select(-.id, -.f)),
      cv_pairs.row = purrr::map(cv_pairs.id, ~ dplyr::filter(pl$cv_pairs, .id == .) %>%
                                  dplyr::select(-.id))
    ) %>%
    tidyr::unnest(models.row, cv_pairs.row) %>%
    # Recover training data
    dplyr::mutate(train = purrr::map2(train, train_p, p_from_resample))
}
