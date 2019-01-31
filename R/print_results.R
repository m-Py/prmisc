
#' Print the results of a t-test
#'
#' @param t_object an object returned by `afex`s ANOVA functions
#' @param d_object An object returned by `effsize::cohen.d`
#' @param decimals how many decimals should be printed
#' @param decimals_p how many decimals should be printed for the p-value
#'     (defaults to 3)
#' @param paired Logical vector of length 1. Was the t-test a
#'     within-subjects comparison? Determines whether Cohen's d is
#'     printed as d_z when TRUE. Defaults to FALSE.
#'
#' @details Note that the R package `papaja` contains functionality for
#'     printing many different statistical tests. This is just for my
#'     personal use that I have used it in recent writeups.
#'
#' @author Martin Papenberg \email{martin.papenberg@@hhu.de}
#' @export
#'
print_ttest <- function(t_object, d_object, decimals=2, decimals_p = 3,
                        paired = FALSE) {
  p <- format_p(t_object$p.value, decimals_p)
  t <- paste0("$t(", round(t_object$parameter, decimals), ") = ")
  t <- paste0(t, force_decimals(t_object$statistic, decimals), "$")
  d <- "$d = "
  if (paired) d <- "$d_z = "
  d <- paste0(d, force_decimals(d_object$estimate, decimals), "$")
  return(paste(t, p, d, sep = ", "))
}

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
#' @param decimals How many decimals should be printed
#' @param decimals_p How many decimals should be printed for the p-value
#'     (defaults to 3)
#'
#' @return A string describing the results of the chi-square test to be
#'   printed in Rmarkdown documents.
#'
#' @author Martin Papenberg \email{martin.papenberg@@hhu.de}
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


#' Format a p-value according to APA standards
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
