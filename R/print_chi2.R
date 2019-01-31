

#' Printing the results of a chi-square test
#'
#' @param tab A contingency table. Do not combine with argument
#'     `chi2.object`.
#' @param chi2.object an object that is returned by `chisq.test`. Do not
#'     combine with argument `tab`. Can also handle objects returned by
#'     `spgs::chisq.unif.test`.
#' @param es Boolean. Should the phi coefficient be printed. This
#'     argument only has an effect if `tab` is passed as a 2x2
#'     contingency table. Does not have an effect if `chi2.object` is
#'     passed.
#' @param correct Boolean. Apply a continuity correction? See
#'     `?chisq.test`. Only has an effect if the chi-square-test
#'     is computed by this function, i.e., if `tab` was passed.
#'     The default value is FALSE.
#' @param decimals How many decimals should be printed
#' @param decimals_p How many decimals should be printed for the p-value
#'     (defaults to 3)
#'
#' @return A string describing the results of the chi-square test to be
#'   printed in Rmarkdown documents.
#'   
#' @examples 
#' 
#' x <- matrix(c(12, 5, 7, 7), ncol = 2)
#' print_chi2(x) # does not use continuity correction by default
#' print_chi2(x, correct = TRUE) # uses continuity correction
#'
#' @author Martin Papenberg \email{martin.papenberg@@hhu.de}
#' 
#' @importFrom stats chisq.test
#' 
#' @export
#'

print_chi2 <- function(tab = NULL, chi2.object = NULL, es = TRUE,
                       correct = FALSE, decimals = 2, decimals_p = 3) {
  ## Test input
  table_passed <- !is.null(tab)
  chi2_passed  <- !is.null(chi2.object)
  is_2x2_contingency <- table_passed & all(dim(tab) == c(2, 2)) & length(dim(tab) == 2)
  if (table_passed & chi2_passed)
    stop("Error: only one of the arguments `tab` and `chi2.object` can be passed.")
  if (!(table_passed | chi2_passed))
    stop("Error: one of the arguments `tab` and `chi2.object` has to be passed.")
  
  N <- ""
  if (table_passed) {
    chi2.object <- chisq.test(tab, correct = correct)
    N <- paste0(", N = ", sum(tab))
  }
  p <- format_p(chi2.object$p.value, decimals_p)
  c <- paste0("$\\chi^2(", chi2.object$parameter[1], N, ") = ")
  c <- paste0(c, force_decimals(chi2.object$statistic, decimals), "$")
  phi <- decimals_only(sqrt(chi2.object$statistic / sum(tab)), decimals)
  phi <- paste0("$\\phi = ", phi, "$")
  
  ## Create return string
  if (es == FALSE | !is_2x2_contingency)
    rtn <- paste(c, p, sep = ", ") # do not print effect size
  if (es == TRUE & is_2x2_contingency)
    rtn <- paste(c, p, phi, sep = ", ") # print effect size for 2x2 contigency tables
  return(rtn)
}
