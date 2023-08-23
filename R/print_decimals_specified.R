
#' Force printing a specified number of decimals for a number
#'
#' @param x the numeric values to be printed
#' @param decimals how many decimals are to be printed. Defaults to 2.
#' @param round_zero whether small values should be rounded to zero or whether 
#'                   they should be displayed as e.g. < .01. See details. 
#'                   Defaults to \code{TRUE}.
#'
#' @return The number in the required format
#' 
#' @details 
#' 
#' By default, \code{force_decimals()} will round numbers that are small enough 
#' down to zero, e.g., 0.0004 will be rounded to 0.00. If 
#' \code{round_zero = FALSE}, \code{force_decimals(0.0004)} will return 
#' \code{"< 0.01"} instead. See examples.
#'
#' @examples
#' 
#' force_decimals(c(1.23456, 0.873, 2.3456))
#' force_decimals(c(1.23456, 0.873, 2.3456), 3)
#' 
#' force_decimals(c(0.004, 0.001, 0.0005, 0.02))
#' force_decimals(c(0.004, 0.001, 0.0005, 0.02), round_zero = FALSE)
#' force_decimals(c(0.004, 0.001, 0.0005, 0.02), 3, round_zero = FALSE)
#' 
#' @author Martin Papenberg \email{martin.papenberg@@hhu.de}
#' @export
#'
force_decimals <- function(x, decimals = 2, round_zero = TRUE) {
  return(vectorize_print(x, decimals, force_decimals_, round_zero))
}

force_decimals_ <- function(x, decimals, round_zero) {
  if (is.na(x)) {
    return(NA_character_)
  }
  if (round(x, decimals) == 0 & !round_zero) {
    return(paste0("< 0.", paste(rep("0", decimals - 1), collapse = ""), "1"))
  }
  return(format(round(x, decimals), nsmall = decimals, scientific = FALSE))
}
