#' Fit mahine learning models
#'
#' \code{pipelearner_fit} takes a pipelearner object and conducts the steps
#' needed to fit all of the specified models.
#'
#' @inheritParams pipelearner_params
#' @export
fit <- function(pl) {
  UseMethod("fit")
}

#' @export
fit.default <- function(pl) {
  stop("fit() should only be used with a pipelearner object")
}

#' @export
fit.pipelearner <- function(pl) {

  if (is.null(pl$models))
    stop("There are no models to fit. Add models first with `learn_models`")

  pl$fits <- purrr::map_df(pl$train_ps, fit_p, pl$cv_pairs, pl$models)

  pl
}


fit_cvdata <- function(train_data, cv_id, models) {
  models %>%
    dplyr::mutate(fit = purrr::invoke_map(.$.f, .$params, data = train_data),
                  cv.id = cv_id)
}

fit_p <- function(p, cv_tbl, models) {
  data_list <- purrr::map(cv_tbl$train, ~ as.data.frame(.) %>% dplyr::sample_frac(p))
  purrr::map2_df(data_list, cv_tbl$.id, fit_cvdata, models = models) %>%
    dplyr::mutate(train.p = p)
}
