#' Fit models in a pipe learner object
#'
#' \code{pipelearner_fit} takes a pipelearner object and conducts the steps
#' needed to fit all of the specified models.
#'
#' @param pl pipelearner object
#' @export
pipelearner_fit <- function(pl) {

  # If required, coerce data frame to default .80 train and .20 test set
  # if (length(pl$data) != 3 ||
  #     all(names(pl$data) != c("train", "test", ".id")) ||
  #     all(purrr::map_chr(pl$data, class) == c("list", "list", "character"))) {
  #
  #   pl$data <- modelr::crossv_mc(pl$data, 1, test = 0.2)
  # }

  pl

}
