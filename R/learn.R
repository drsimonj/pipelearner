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
  pl$fits <- purrr::map_df(pl$train_ps, fit_p, pl$cv_pairs, pl$models)

  pl
}


fit_cvdata <- function(train_data, cv_id, models) {
  tibble::tibble(
    cv_pair = cv_id,
    model   = models$.id,
    fit     = purrr::invoke_map(models$.f, models$params, data = train_data),
    true_train_target = purrr::map(models$target, ~ train_data[[.]])
  )
}

fit_p <- function(p, cv_tbl, models) {
  # Fit models
  data_list <- purrr::map(cv_tbl$train, ~ as.data.frame(.) %>% dplyr::sample_frac(p))

  to_fit <- list(train_data = data_list, cv_id = cv_tbl$.id)

  tmp <- purrr::pmap_df(to_fit, fit_cvdata, models = models) %>%
          dplyr::mutate(train_p = p)


  #test_data = cv_tbl$test,

  # Order columns and arrange rows
  tmp %>%
    dplyr::select(model, cv_pair, train_p, dplyr::everything()) %>%
    dplyr::arrange(model, cv_pair, train_p)

}
