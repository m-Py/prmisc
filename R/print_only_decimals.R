
#' Force printing a specified number of decimals and leave out a leading
#' zero
#'
#' @param x the values to be printed
#' @param decimals how many decimals are to be printed
#'
#' @return The number in the required format
#'
#' @author Martin Papenberg \email{martin.papenberg@@hhu.de}
#' @export
#'
decimals_only <- function(x, decimals) {
  return(vectorize_print(x, decimals, decimals_only_))
}

decimals_only_ <- function(x, decimals) {
  x <- as.numeric(x)
  n_small <- force_decimals(x, decimals)
  cut_decimal <- paste0(".", strsplit(as.character(n_small), ".", TRUE)[[1]][2])
  if (x < 0) cut_decimal <- paste0("-", cut_decimal)
  return(cut_decimal)
}
