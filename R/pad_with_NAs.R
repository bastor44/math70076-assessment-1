#' Function to pad a vector with NAs to be used in the rolling_means function
#'
#' @param x Vector to which NAs will be added
#' @param n_left Number of NAs to add before x.
#' @param n_right Number of NAs to add after x.
#'
#' @returns A vector containing x with the requested number of NA values before and after.
#'
#' @export
#' @examples
#' pad_with_NAs(1:5, n_left = 0, n_right = 3)
#' pad_with_NAs(c("spider", "mouse", "cat", "dog"), n_left = 1, n_right = 2)
#'
pad_with_NAs <- function(x, n_left, n_right){
  c(rep(NA, n_left), x, rep(NA, n_right))
}
