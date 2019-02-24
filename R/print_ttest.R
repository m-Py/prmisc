

#' Print the results of a t-test
#'
#' @param t_object An object returned by \code{\link{t.test}}
#' @param d_object An object returned by \code{\link[effsize]{cohen.d}}
#' @param decimals How many decimals should be printed for r and t-values.
#' @param decimals_p How many decimals should be printed for the p-value
#'     (defaults to 3)
#' @param paired Logical vector of length 1. Was the t-test a
#'     within-subjects comparison? Determines whether Cohen's d is
#'     printed as d_z when \code{TRUE}. Defaults to \code{FALSE}.
#'     
#' @return A string describing the t-test; to be 
#'     included in an R markdown document.
#'
#' @details 
#' 
#' To use this function, you need to install the R package effsize and 
#' compute a Cohen's d object; pass this object as the second argument.
#' 
#' @references 
#' Cohen, J. (1988). Statistical power analysis for the behavioral sciences
#'     (2nd ed.). Hillsale, NJ: Lawrence Erlbaum.
#'     
#' Torchiano, M. (2018). effsize: Efficient Effect Size Computation. 
#'     https://CRAN.R-project.org/package=effsize
#'
#' @examples 
#' 
#' ttest <- t.test(1:10, y = c(7:20), var.equal = TRUE)
#' library("effsize") # for Cohen's d
#' cohend <- cohen.d(1:10, c(7:20))
#' print_ttest(ttest, cohend) # include this call in Rmd inline code
#'
#' @author Martin Papenberg \email{martin.papenberg@@hhu.de}
#' @export
#'
print_ttest <- function(t_object, d_object, decimals=2, decimals_p = 3,
                        paired = FALSE) {
  validate_input(t_object, "t_object", "htest")
  validate_input(d_object, "d_object", "effsize")
  validate_input(decimals, "decimals",  c("numeric", "integer"), 1, TRUE)
  validate_input(decimals_p, "decimals_p", c("numeric", "integer"), 1, TRUE)
  validate_input(paired, "paired", "logical", 1)
  
  p <- format_p(t_object$p.value, decimals_p)
  t <- paste0("$t(", round(t_object$parameter, decimals), ") = ")
  t <- paste0(t, force_decimals(t_object$statistic, decimals), "$")
  d <- "$d = "
  if (paired) d <- "$d_z = "
  d <- paste0(d, force_decimals(d_object$estimate, decimals), "$")
  return(paste(t, p, d, sep = ", "))
}
