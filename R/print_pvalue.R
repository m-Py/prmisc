


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