
#' Print the results of a chi-square test
#'
#' @param x A contingency table (passed as \code{table} or \code{matrix}) or
#'     an object of type "htest" returned by \code{\link{chisq.test}}. Can 
#'     also handle objects returned by \code{\link[spgs]{chisq.unif.test}}
#'     from the \code{spgs} package.
#' @param es Boolean. Should the phi coefficient be printed as a 
#'     measure of effect size. See details.
#' @param correct Boolean. Apply a continuity correction? See
#'     \code{\link{chisq.test}}. Only has an effect if the chi-square-test
#'     is computed by this function, i.e., if \code{x} is a contingency 
#'     table. The default value is \code{FALSE}.
#' @param decimals How many decimals should be printed
#' @param decimals_p How many decimals should be printed for the p-value
#'     (defaults to 3)
#'
#' @return A string describing the results of the chi-square test to be
#'     printed in Rmarkdown documents.
#'     
#' @details 
#' 
#' The argument \code{es} only has an effect if \code{x} is passed as a 2x2
#' contingency table. In this case, the phi coefficient is computed as 
#' a measure of effect size (see Cohen, 1988, page 223).
#' 
#' @references 
#' Cohen, J. (1988). Statistical power analysis for the behavioral sciences
#'     (2nd ed.). Hillsale, NJ: Lawrence Erlbaum.
#'   
#' @examples 
#' 
#' # Pass a matrix
#' x <- matrix(c(12, 5, 7, 7), ncol = 2)
#' print_chi2(x) # does not use continuity correction by default
#' print_chi2(x, correct = TRUE) # uses continuity correction
#' 
#' # Pass a table
#' tab <- table(rbinom(150, 1, 0.5), rbinom(150, 1, 0.1))
#' print_chi2(tab, correct = FALSE)
#' 
#' # Pass a chi-squared test object
#' print_chi2(chisq.test(tab, correct = FALSE))
#' 
#'
#' @author Martin Papenberg \email{martin.papenberg@@hhu.de}
#' 
#' @importFrom stats chisq.test
#' 
#' @export
#'

print_chi2 <- function(x, es = TRUE, correct = FALSE, decimals = 2, decimals_p = 3) {
  ## Test input
  validate_input(x, "x", c("table", "matrix", "htest"))
  validate_input(es, "es", "logical", 1)
  validate_input(correct, "correct", "logical", 1)
  validate_input(decimals, "decimals",  "numeric", 1, TRUE, TRUE)
  validate_input(decimals_p, "decimals_p", "numeric", 1, TRUE, TRUE)
  if (inherits(x ,"htest")) {
    return(print_chi2_(NULL, x, es, correct, decimals, decimals_p))
  } else {
    return(print_chi2_(x, NULL, es, correct, decimals, decimals_p))
  }
}

# The internal function that does all the work for print_chi2
print_chi2_ <- function(tab = NULL, chi2.object = NULL, es,
                        correct, decimals, decimals_p) {
  
  # What argument was passed?
  table_passed <- !is.null(tab)
  is_2x2_contingency <- table_passed & all(dim(tab) == c(2, 2)) & length(dim(tab) == 2)

  N <- ""
  if (table_passed) {
    chi2.object <- chisq.test(tab, correct = correct)
    N <- paste0(", N = ", sum(tab))
  }
  
  p <- format_p(chi2.object$p.value, decimals_p)
  c <- paste0("$\\chi^2(", chi2.object$parameter[1], N, ") = ")
  c <- paste0(c, force_decimals(chi2.object$statistic, decimals), "$")

  ## Create return string
  if (es == FALSE | !is_2x2_contingency) {
    rtn <- paste(c, p, sep = ", ") # do not print effect size
  }
  # print effect size for 2x2 contigency tables
  if (es == TRUE & is_2x2_contingency) {
    phi <- decimals_only(sqrt(chi2.object$statistic / sum(tab)), decimals)
    phi <- paste0("$\\phi = ", phi, "$")
    rtn <- paste(c, p, phi, sep = ", ") 
  }
  return(rtn)
}
