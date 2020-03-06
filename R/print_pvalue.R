

#' Format a p-value according to APA standards
#' 
#' @param pvalues The p-values
#' @param decimals The number of decimals to be printed
#' @param numbers_only Logical, indicates whether the p-values
#'                     should be printed whithout the accompanying p.
#'                     Defaults to \code{FALSE}. 
#'
#' @return A string representation of the p value to be used in Rmarkdown
#'   documents.
#' 
#' @export
#' 
#' @examples
#' 
#' # Format a p-value, default is 3 decimals
#' format_p(0.03123)
#' format_p(0.000001231)
#' format_p(0.000001231, decimals = 2)
#' format_p(0.3123, decimals = 2)
#' # Format several p-values with one function call
#' format_p(c(0.3123, 0.001, 0.00001, 0.19))
#' format_p(c(.999, .9999, 1))
#' format_p(c(0.3123, 0.001, 0.00001, 0.19, .99999), numbers_only = TRUE)
#'

format_p <- function(pvalues, decimals = 3, numbers_only = FALSE) {
  return(vectorize_print(pvalues, decimals, format_p_, numbers_only))
}

format_p_ <- function(pvalue, decimals, numbers_only) {
  if (is.na(pvalue)) {
    return(as.character(NA))
  }
  if (pvalue < 0 | pvalue > 1) {
    stop("p value is smaller than 0 or larger than 1")
  }
  if (pvalue >= 0.001) {
    p <- paste0("$p = ", decimals_only(pvalue, decimals), "$")
  }
  ## Special case: p-value is 1 (or the rounded p-value is 1)
  if (round(pvalue, decimals) == 1) {
    p <- paste0("$p > .", paste0(rep("9", decimals), collapse = ""), "$")
  }
  if (pvalue < 0.01 & decimals <= 2) {
    p <- "$p < .01$"
  }
  if (pvalue < 0.001) {
    p <- "$p < .001$"
  }
  if (numbers_only) {
    p <- gsub("p = ", "", p)
    p <- gsub("p ", "", p)
    return(p)
  }
  return(p)
}
