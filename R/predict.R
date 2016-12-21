#' Predictions for fitted learning models in a pipelearner object
#'
#' Produces true and predicted values for all training and test sets, for all
#' models, in a pipelearner object.
#'
#' @inheritParams pipelearner_params
#' @export
predict.pipelearner <- function(pl) {
  for_pred <- recover_fits(pl)


}

