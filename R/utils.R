#' Pipe operator
#'
#' @name %>%
#' @rdname pipe
#' @keywords internal
#' @export
#' @importFrom magrittr %>%
#' @usage lhs \%>\% rhs
NULL


#' Proportion of resample object
#'
#' Create a new resample object from the rounded proportion of indices starting
#' at the beginning of a resample object. For example, create a resample object
#' pointing to the the first .3 (30\%) or .5 (50\%) of rows pointed to by the
#' original resample object.
#'
#' @param resample \code{\link[modelr]{resample}} object
#' @param p Non-zero proportion.
#' @export
p_from_resample <- function(resample, p) {
  n = round(nrow(resample) * p)  # Rounded
  resample$idx <- resample$idx[seq_len(n)]
  resample
}
