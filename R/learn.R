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
    dplyr::mutate(.id = as.character(seq_len(nrow(.))))

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
