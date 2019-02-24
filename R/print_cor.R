
#' Printing the results of a significance test for a correlation
#' coefficient
#'
#' @param cor_object An object returned by `cor.test`
#' @param print_t Boolean, should the t-statistic be printed. 
#'     (defaults to `FALSE`)
#' @param decimals how many decimals should be printed
#' @param decimals_p how many decimals should be printed for the p-value
#'     (defaults to 3)
#'
#' @return A string describing the test
#' 
#' @examples 
#' 
#' x <- c(44.4, 45.9, 41.9, 53.3, 44.7, 44.1, 50.7, 45.2, 60.1)
#' y <- c( 2.6,  3.1,  2.5,  5.0,  3.6,  4.0,  5.2,  2.8,  3.8)
#' cor_results <- cor.test(x, y)
#' print_cortest(cor_results)
#' print_cortest(cor_results, print_t = TRUE)
#'
#' @author Martin Papenberg \email{martin.papenberg@@hhu.de}
#' @export
#' 
print_cortest <- function(cor_object, print_t = FALSE, decimals = 2, 
                          decimals_p = 3) {
  p <- format_p(cor_object$p.value, decimals_p)
  t <- paste0("$t = ", force_decimals(cor_object$statistic, decimals), "$")
  cor <- paste0("$r = ", decimals_only(cor_object$estimate, decimals), "$")
  rtn <- paste(cor, p, sep = ", ")
  if (print_t == TRUE) {
    rtn <- paste(cor, t, p, sep = ", ")
  }
  return(rtn)
}
