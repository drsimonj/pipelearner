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
  stop("learn() should only be used with a pipelearner object")
}

#' @export
learn.pipelearner <- function(pl) {

  if (is.null(pl$models))
    stop("There are no models to learn with. Add models first with `learn_models`")

  pl$fits <- purrr::map_df(pl$train_ps, fit_p, pl$cv_pairs, pl$models)

  pl
}


fit_cvdata <- function(train_data, test_data, cv_id, models) {
  models %>%
    dplyr::mutate(fit = purrr::invoke_map(.$.f, .$params, data = train_data),
                  predicted_train = purrr::map(fit, predict, newdata = train_data),
                  predicted_test  = purrr::map(fit, predict, newdata = test_data),
                 #outcome_var = purrr::map_chr(.$params, ~ as.character(lazyeval::f_lhs(.$formula))),
                  cv_pair = cv_id)
}

fit_p <- function(p, cv_tbl, models) {
  data_list <- purrr::map(cv_tbl$train, ~ as.data.frame(.) %>% dplyr::sample_frac(p))
  #purrr::map2_df(data_list, cv_tbl$.id, fit_cvdata, models = models) %>%
  to_fit <- list(train_data = data_list, test_data = cv_tbl$test, cv_id = cv_tbl$.id)
  purrr::pmap_df(to_fit, fit_cvdata, models = models) %>%
    dplyr::mutate(train_p = p)

}
