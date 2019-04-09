
#' Printing the results of a significance test for a correlation
#' coefficient
#'
#' @param cor_object An object of class "htest" returned by 
#'     \code{\link{cor.test}}
#' @param decimals How many decimals should be printed for the test
#'     statistic (defaults to 2).
#' @param decimals_p How many decimals should be printed for the p value
#'     (defaults to 3).
#'
#' @return A string describing the significance test; to be 
#'     included in an R markdown document.
#' 
#' @examples 
#' 
#' x <- c(44.4, 45.9, 41.9, 53.3, 44.7, 44.1, 50.7, 45.2, 60.1)
#' y <- c( 2.6,  3.1,  2.5,  5.0,  3.6,  4.0,  5.2,  2.8,  3.8)
#' cor_results <- cor.test(x, y)
#' print_cortest(cor_results)
#'
#' @author Martin Papenberg \email{martin.papenberg@@hhu.de}
#' @export
#' 
print_cortest <- function(cor_object, decimals = 2, decimals_p = 3) {
  validate_input(cor_object, "cor_object", "htest")
  validate_input(decimals, "decimals",  "numeric", 1, TRUE, TRUE)
  validate_input(decimals_p, "decimals_p", "numeric", 1, TRUE, TRUE)
  p <- format_p(cor_object$p.value, decimals_p)
  df <- paste0("(", cor_object$parameter, ") = ")
  
  if (cor_object$method == "Pearson's product-moment correlation") {
    cor <- paste0("$r", df, decimals_only(cor_object$estimate, decimals), "$")
  } else if (cor_object$method == "Kendall's rank correlation tau") {
    cor <- paste0("$\\tau_b", decimals_only(cor_object$estimate, decimals), "$")
  } else if (cor_object$method == "Spearman's rank correlation rho") {
    cor <- paste0("$r_s", decimals_only(cor_object$estimate, decimals), "$")
  }
  
  return(paste(cor, p, sep = ", "))
}
