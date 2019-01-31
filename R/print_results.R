

#' Format a p-value according to APA standards
#' 
#' @param pvalues The p-values
#' @param decimals The number of decimals to be printed
#' @return A string representation of the p value to be used in Rmarkdown
#'   documents.
#' 
#' @export
#'
format_p <- function(pvalues, decimals = 3) {
  return(vectorize_print(pvalues, decimals, format_p_))
}

format_p_ <- function(pvalue, decimals) {
  if (pvalue < 0 | pvalue > 1)
    stop("p value is smaller than 0 or larger than 1")
  if (pvalue >= 0.001) {
    p <- paste0("$p = ", decimals_only(pvalue, decimals), "$")
  }
  ## Special case: p-value is 1 (or the rounded p-value is 1)
  if (round(pvalue, decimals) == 1) {
    p <- paste0("$p > .", paste0(rep("9", decimals), collapse = ""), "$")
  }
  if (pvalue < 0.005 & decimals <= 2) p <- "$p < .01$"
  if (pvalue < 0.001) p <- "$p < .001$"
  return(p)
}


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
