#' Learn from data
#'
#' \code{learn} takes a pipelearner object and conducts the steps needed to
#' learn/fit all of the specified models on data.
#'
#' @inheritParams pipelearner_params
#' @export
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

  # Fit models
  pl$fits <- purrr::map_df(pl$train_ps, fit_p, pl$cv_pairs, pl$models) %>%
    # Arrange columns and rows
    dplyr::select(models.id, cv_pairs.id, train_p, dplyr::everything()) %>%
    dplyr::arrange(models.id, cv_pairs.id, train_p) %>%
    # Add .id
    dplyr::mutate(.id = seq_len(nrow(.)))

  pl
}


fit_cvdata <- function(train_data, cv_id, models) {
  tibble::tibble(
    cv_pairs.id = cv_id,
    models.id   = models$.id,
    fit         = purrr::invoke_map(models$.f, models$params, data = train_data)
  )
}

fit_p <- function(p, cv_tbl, models) {
  # Fit models
  data_list <- purrr::map(cv_tbl$train, p_from_resample, p)

  to_fit <- list(train_data = data_list, cv_id = cv_tbl$.id)

  purrr::pmap_df(to_fit, fit_cvdata, models = models) %>%
    dplyr::mutate(train_p = p)

}

#' Recover model and cross-validation information for fits
#'
#' \code{recover_fits} will use the models and cv_pairs `.id` references from a
#' fits tibble in a pipelearner object to recover relevant information.
#'
#' @inheritParams pipelearner_params
#' @export
recover_fits <- function(pl) {
  UseMethod("recover_fits")
}

#' @export
recover_fits.default <- function(pl) {
  stop("`recover_fits` should only be used with a pipelearner object")
}

#' @export
recover_fits.pipelearner <- function(pl) {
  if (is.null(pl$fits)) stop ("Models haven't been fitted. Find help at ?learn")

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
    dplyr::mutate(train = purrr::map2(train, train_p, p_from_resample)) %>%
    # Order columns
    dplyr::select(-.id, dplyr::everything(), .id)
}
