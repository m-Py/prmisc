
#' Force printing a specified number of decimals and leave out a leading
#' zero
#'
#' @param x the values to be printed
#' @param decimals how many decimals are to be printed. Defaults to 2.
#'
#' @return The number in the required format
#' 
#' @examples 
#' 
#' decimals_only(c(0.23456, 0.873, 0.3456), decimals = 3)
#'
#' @author Martin Papenberg \email{martin.papenberg@@hhu.de}
#' @export
#'
decimals_only <- function(x, decimals = 2) {
  if (any(x > 1))
    warning("At least one number was greater than one, its leading digit was left intact.")
  return(vectorize_print(x, decimals, decimals_only_))
}

decimals_only_ <- function(x, decimals) {
  if (x >= 1)
    return(force_or_cut(x, decimals))
  x <- as.numeric(x)
  n_small <- force_decimals(x, decimals)
  cut_decimal <- paste0(".", strsplit(as.character(n_small), ".", TRUE)[[1]][2])
  if (x < 0) cut_decimal <- paste0("-", cut_decimal)
  return(cut_decimal)
}
