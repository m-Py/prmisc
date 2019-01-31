
#' Printing the results of a significance test for a correlation
#' coefficient
#'
#' @param cor_object An object returned by `cor.test`
#' @param decimals how many decimals should be printed
#' @param decimals_p how many decimals should be printed for the p-value
#'     (defaults to 3)
#'
#' @return A string describing the test
#'
#' @author Martin Papenberg \email{martin.papenberg@@hhu.de}
#' @export
#' 
print_cortest <- function(cor_object, decimals = 2, decimals_p = 3) {
  p <- format_p(cor_object$p.value, decimals_p)
  cor <- paste0("$r = ", decimals_only(cor_object$estimate, decimals), "$")
  rtn <- paste(cor, p, sep = ", ")
  return(rtn)
}
