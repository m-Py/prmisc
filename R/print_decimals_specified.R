
#' Force printing a specified number of decimals for a number
#'
#' @param x the values to be printed
#' @param decimals how many decimals are to be printed
#'
#' @return The number in the required format
#'
#' @author Martin Papenberg \email{martin.papenberg@@hhu.de}
#' @export
#'
force_decimals <- function(x, decimals) {
  return(vectorize_print(x, decimals, force_decimals_))
}

force_decimals_ <- function(x, decimals) {
  x <- as.numeric(x)
  return(format(round(x, decimals), nsmall = decimals))
}
