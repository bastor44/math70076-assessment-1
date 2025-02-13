#' Rolling Mean function based on the one written in class
#'
#' @param x numeric vector
#' @param window_width number of values included in each mean calculation.
#'                     should be an odd, positive integer
#' @param window_position indicate if window should be left justified, centred,
#'                        or right justified
#' @param ... Additional arguments to pass to the mean() function call
#'
#' @returns a vector rolling mean values of the same length as `x`
#' @export
#'
#' @examples
#' rolling_mean(x=1:5, window_width=3)
#' rolling_mean(x=1:5, window_width=5)
#' rolling_mean(x=c(TRUE, TRUE, TRUE, FALSE, TRUE, TRUE, TRUE), window_width=3)
#'
#'
rolling_mean <- function(x, window_width, window_position="centre") {
  # ----- Input Checks ---------------------------------------------------------
  # Check x is a vector with numeric interpretation
  stopifnot(is.logical(x) | is.integer(x) | is.double(x) | is.complex(x))
  stopifnot(length(x) > 0)
  
  # Check window width is an odd, positive integer
  stopifnot(length(window_width)==1) # one number
  stopifnot(window_width %% 1 == 0 ) # integer
  stopifnot((window_width/2) %% 1 !=0) # odd
  stopifnot(window_width > 0) # positive
  
  # ----- Function Body --------------------------------------------------------
  # number of values left and right to include
  half_width <- floor(window_width/2)
  x_padded <- pad_with_NAs(x, n_left = half_width, n_right=half_width)
  evaluation_locations <- seq_along(x) + half_width
  
  output <- rep(NA, length(x))
  
  for (index in evaluation_locations) {
    # extract relevant values from x_padded
    indices_in_window <- seq(index-half_width, index+half_width, by=1)
    values_in_window <- x_padded[indices_in_window]
    
    # calculate and store mean
    output[index - half_width] <- mean(values_in_window)
  }
  
  return(output)
}
