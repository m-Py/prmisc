
#' Print a number having a specified number of digits or as integer
#'
#' @param x A vector of numbers
#' @param decimals The number of digits that should be printed if x is a
#'     decimal number
#'
#' @return The number in the required format
#'
#' @details If x integer, only the integer is printed, if x is a decimal
#'     number, the decimals are printed
#'
#' @author Martin Papenberg \email{martin.papenberg@@hhu.de}
#' @export
#'
force_or_cut <- function(x, decimals) {
  return(vectorize_print(x, decimals, force_or_cut_))
}

force_or_cut_ <- function(x, decimals) {
  x <- as.numeric(x)
  if (x %% 1 == 0) return(as.character(x))
  else return(force_decimals_(x, decimals))
}

## An abstract function used to vectorize all number printing functions
vectorize_print <- function(x, decimals, FUN) {
  x_ <- vapply(x, FUN, FUN.VALUE = "character", decimals)
  return(x_)
}
