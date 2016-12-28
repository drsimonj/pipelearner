#' Summarize a pipelearner object
#'
#' Summarize all fitted models in a pipelearner object
#'
#' @inheritParams pipelearner_params
#' @export
summary.pipelearner <- function(pl) {
  recover_fits.pipelearner(pl)
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
