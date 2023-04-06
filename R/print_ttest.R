

#' Print the results of a t-test
#'
#' @param t_object An object of class "htest" returned by 
#'     \code{\link{t.test}}.
#' @param d_object An effect size table returned by 
#'     \code{\link[effectsize]{cohens_d}} from package \code{effectsize}. 
#'     Optional argument.
#' @param decimals How many decimals should be printed for the t-value
#'     (defaults to 2).
#' @param decimals_p How many decimals should be printed for the p-value
#'     (defaults to 3).
#' @param confidence Logical. Whether a confidence interval for the effectsize 
#'     should be printed or not. Can only be \code{TRUE} if \code{d_object} is
#'     provided.
#'     
#' @return A string describing the t-test; to be 
#'     included in an R markdown document.
#'
#' @details 
#' 
#' To use this function, you need to install the R package \code{effectsize}
#' to compute Cohen's d; pass this object as the second argument.
#' 
#' @references 
#' Cohen, J. (1988). Statistical power analysis for the behavioral sciences
#'     (2nd ed.). Hillsale, NJ: Lawrence Erlbaum.
#'
#' @examples 
#' 
#' ttest <- t.test(1:10, y = c(7:20), var.equal = TRUE)
#' library("effectsize") # for Cohen's d
#' cohend <- cohens_d(1:10, c(7:20))
#' print_ttest(ttest, cohend) # include this call in Rmd inline code
#' 
#' # An example for paired data:
#' data(sleep) # ?sleep
#' tt <- t.test(sleep$extra[sleep$group == 1], 
#'              sleep$extra[sleep$group == 2], paired = TRUE)
#' cd <- cohens_d(sleep$extra[sleep$group == 1], 
#'                sleep$extra[sleep$group == 2], paired = TRUE)
#' print_ttest(tt, cd)
#' # Print the confidence interval
#' print_ttest(tt, cd, confidence = TRUE)
#' # The information about the CI is taken from the effectsize object:
#' cd <- cohens_d(sleep$extra[sleep$group == 1], 
#'                sleep$extra[sleep$group == 2], paired = TRUE, ci = .8)
#' print_ttest(tt, cd, confidence = TRUE)
#' # effect size object can be left out:
#' print_ttest(tt)
#'
#' @author Martin Papenberg \email{martin.papenberg@@hhu.de}
#' @export
#'
print_ttest <- function(t_object, d_object = NULL, decimals=2, decimals_p = 3, confidence = FALSE) {
  validate_input(t_object, "t_object", "htest")
  if (!is.null(d_object)) {
    # Validate input
    validate_input(
      d_object, 
      "d_object",
      c("effectsize_difference","effectsize_table",
        "see_effectsize_table", "data.frame")
    )
    # Validate whether t-test and Cohen's d are both (not) paired
    t_paired <- ifelse(t_object$method == "Paired t-test", "paired", "not paired")
    d_paired <- ifelse(attr(d_object, "paired"), "paired", "not paired")
    
    if (t_paired != d_paired) {
      stop(paste0("t-test is ", t_paired, ", but Cohen's d is ", d_paired))
    }
  } else {
    if (confidence) stop("Cannot print confidence intervals when no d_object is provided.")
  }
  
  validate_input(decimals, "decimals",  "numeric", 1, TRUE, TRUE)
  validate_input(decimals_p, "decimals_p", "numeric", 1, TRUE, TRUE)
  
  p <- format_p(t_object$p.value, decimals_p)
  t <- paste0("$t(", round(t_object$parameter, decimals), ") = ")
  t <- paste0(t, force_decimals(t_object$statistic, decimals), "$")
  if (!is.null(d_object)) {
    d <- "$d = "
    if (attr(d_object, "paired")) d <- "$d_z = "
    d <- paste0(d, force_decimals(d_object$Cohens_d, decimals), "$")
    
    if (confidence) {
      conf_int <- 
        paste0(
          "$", d_object$CI * 100, "% CI [", force_decimals(d_object$CI_low), ", ", 
          force_decimals(d_object$CI_high),  "]$"
        )
      return(paste(t, p, d, conf_int, sep = ", "))
    }
    
    return(paste(t, p, d, sep = ", "))
  }
  return(paste(t, p, sep = ", "))
}

