#' Utility function to round to 2 d.p.
#'
#' @param x Object to round
#' @return Input rounded to 2 dp
#' @export
round_2_dp <- function(x){
  format(round(as.numeric(x), digits = 2), nsmall = 2)
}
