#' Predictions for fitted learning models in a pipelearner object
#'
#' Produces true and predicted values for all training and test sets, for all
#' models, in a pipelearner object.
#'
#' @inheritParams pipelearner_params
#' @export
predict.pipelearner <- function(pl) {

  if (is.null(pl$fits)) stop ("Models haven't learned yet. See ?learn")

  to_pred <- pl %>% recover_fits()

  to_pred %>%
    # Get results
    dplyr::select(fit, target, train, test, .id) %>%
    purrr::pmap_df(make_predictions) %>%
    # Tidy format
    tidyr::gather(key, val, -fits.id) %>%
    tidyr::separate(key, into = c("origin", "data")) %>%
    tidyr::spread(origin, val) %>%
    tidyr::unnest(predicted, true) %>%
    # Append fits information
    dplyr::mutate(
      fits_info = purrr::map(fits.id, ~ dplyr::filter(to_pred, .id == .) %>%
                               dplyr::select(model, target, params, train_p))
    ) %>%
    tidyr::unnest(fits_info) %>%
    # Arrange column
    dplyr::select(model, target, params, train_p,
                  data, true, predicted, fits.id,
                  dplyr::everything())
}

make_predictions <- function(fit, target, train, test, .id) {
  tibble::tibble(
    true_train = list(as.data.frame(train)[[target]]),
    true_test  = list(as.data.frame(test)[[target]]),
    predicted_train = list(predict(fit)),
    predicted_test  = list(predict(fit, newdata = test)),
    fits.id = .id
  )
}
