
#' Print the results of an `afex` ANOVA object
#'
#' @param afex_object an object returned by `afex`s ANOVA functions
#' @param row the number of the row of the effect in the table view of
#'     the `afex_object`
#' @param es A String "pes" or "ges"; which effect size measure was
#'     used?
#' @param font should the effect size symbol eta be printed in "italic"
#'     or "nonitalic"
#' @param decimals how many decimals should be printed
#' @param decimals_p how many decimals should be printed for the p-value
#'     (defaults to 3)
#'
#'
#' @author Martin Papenberg \email{martin.papenberg@@hhu.de}
#' @export
#' 
print_anova <- function(afex_object, row, es, font="nonitalic", 
                        decimals=2, decimals_p = 3) {
    
    aov.table <- afex_object$anova # contains the relevant values
    
    # Print p-value
    p_value <- aov.table[row,"Pr(>F)"]
    
    if (p_value >= 0.001) {
        p <- paste("$p = ", decimals_only(p_value, decimals_p), "$", sep="")
    }
    if (p_value < 0.005 & decimals_p <= 2) {
        p <- "$p < .01$"
    }
    if (p_value < 0.001) {
        p <- "$p < .001$"
    }

    if (es == "pes") es.symbol <- "p"
    else if (es == "ges") es.symbol <- "G"
    
    # Print F-value
    F <- paste("$F(", force_or_cut(aov.table[row,"num Df"], decimals), "$, $", 
               force_or_cut(aov.table[row,"den Df"], decimals), ") = ", 
               force_decimals(aov.table[row,"F"], decimals), "$", sep="")
    
    # Print eta^2; either according to APA-style nonitalic (using font="nonitalic"),
    # or using font = "italic"; font = "nonitalic" requires latex 
    # package \upgreek (for \upeta)
    if (font == "nonitalic") {
        eta_symbol <- paste("$\\upeta_\\mathrm{", es.symbol ,"}^2 = ", sep="")
    } else if (font == "italic") {
        eta_symbol <- paste("$\\eta_", es.symbol, "^2 = ", sep="")
    } else {
        stop("error in function print_anova: argument 'font' must be 
             'nonitalic' or 'italic'")
    }
    eta <- paste(eta_symbol, decimals_only(aov.table[row,es], decimals), 
                 "$", sep="")
    return(paste(F, p, eta, sep=", "))
}

#' Print the results of a t-test 
#'
#' @param t_object an object returned by `afex`s ANOVA functions
#' @param d_object An object returned by `effsize::cohen.d`
#' @param decimals how many decimals should be printed
#' @param decimals_p how many decimals should be printed for the p-value
#'     (defaults to 3)
#' @param within Logical vector of length 1. Was the t-test a
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
                        within = FALSE) {
    
    if (t_object$p.value >= 0.001) {
        p <- paste("$p = ", decimals_only(t_object$p.value, decimals_p), 
                   "$", sep="")
    }
    if (t_object$p.value < 0.005 & decimals_p <= 2) p <- "$p < .01$"
    if (t_object$p.value < 0.001) p <- "$p < .001$"
    
    t <- paste("$t(", round(t_object$parameter, decimals), ") = ", sep="")
    t <- paste(t, force_decimals(t_object$statistic, decimals), "$", sep="")
    d <- "$d = "
    if (within) d <- "$d_z = "
    d <- paste(d, force_decimals(d_object$estimate, decimals), "$", 
               sep="")
    return(paste(t, p, d, sep=", "))
}

#' Printing the results of a chisquare test
#'
#' @param table a contingency table
#' @param es Boolean. Should the phi coefficient be printed (only makes
#'     sense for 2x2 contingency tables!)
#' @param chi2.object an object that is returned by `chisq.test`
#' @param decimals how many decimals should be printed
#' @param decimals_p how many decimals should be printed for the p-value
#'     (defaults to 3)
#'
#' @return A string describing the chi-squared test
#'
#' @details The returned effect size (the phi-coefficient) only makes
#'     sense for 2x2 contingency tables
#' 
#' @author Martin Papenberg \email{martin.papenberg@@hhu.de}
#' @export
#' 

print_chi2 <- function(table=NULL, es=TRUE, chi2.object=NULL, 
                       decimals=2, decimals_p = 3) {
    N <- ""
    if (!is.null(table)) {
        chi2.object <- chisq.test(table, correct=FALSE)
        N <- paste(", N = ", sum(table), sep="")
    }
    
    if (chi2.object$p.value >= 0.001) {
        p <- paste("$p = ", decimals_only(chi2.object$p.value, 3), 
                   "$", sep="")
    }
    if (chi2.object$p.value < 0.005 & decimals_p <= 2) p <- "$p < .01$"
    if (chi2.object$p.value < 0.001) p <- "$p < .001$"
    
    c <- paste("$\\chi^2(", chi2.object$parameter, N, 
               ") = ", sep="")
    c <- paste(c, force_decimals(chi2.object$statistic, decimals), "$", sep="")
    phi <- paste("$\\phi = ",
                 decimals_only(sqrt(chi2.object$statistic/sum(table)), 
                               decimals),
      "$", sep="")
    if (es == FALSE) rtn <- paste(c, p, sep=", ") # do not print effect size
    if (es == TRUE)  rtn <- paste(c, p, phi, sep=", ")
    # effect size something!
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
print_cortest <- function(cor_object, decimals=2, decimals_p = 3) {
    if (cor_object$p.value >= 0.001) {
        p <- paste("$p = ", decimals_only(cor_object$p.value, decimals_p), 
                   "$", sep="")
    }

    if (cor_object$p.value < 0.005 & decimals_p <= 2) p <- "$p < .01$"
    if (cor_object$p.value < 0.001) p <- "$p < .001$"    
    
    # for *r*, remove 0. before estimate
    cor <- paste("$r = ", decimals_only(cor_object$estimate, decimals), 
                 "$", sep="")
    rtn <- paste(cor, p, sep=", ")
    return(rtn)
}

#' Force printing a specified number of decimals for a number
#'
#' @param x the values to be printed
#' @param n_digits how many decimals are to be printed
#'
#' @return The number in the required format
#' 
#' @author Martin Papenberg \email{martin.papenberg@@hhu.de}
#' @export
#'
force_decimals <- function(x, n_digits) {
    return(vectorize_print(x, n_digits, force_decimals_))
}

force_decimals_ <- function(x, n_digits) {
    x <- as.numeric(x)
    return(format(round(x, n_digits), nsmall = n_digits))
}

#' Force printing a specified number of decimals and leave out a leading
#' zero
#'
#' @param x the values to be printed
#' @param n_digits how many decimals are to be printed
#'
#' @return The number in the required format
#'
#' @author Martin Papenberg \email{martin.papenberg@@hhu.de}
#' @export
#'
decimals_only <- function(x, n_digits) {
    return(vectorize_print(x, n_digits, decimals_only_))
}

decimals_only_ <- function(x, n_digits) {
    x <- as.numeric(x)
    n_small <- force_decimals(x, n_digits)
    cut_decimal <- paste(".", strsplit(as.character(n_small), ".", 
                         TRUE)[[1]][2], sep="")
    if (x < 0) cut_decimal <- paste("-", cut_decimal, sep="")
    return(cut_decimal)
}

#' Print a number having a specified number of digits or as integer
#'
#' @param x A vector of numbers
#' @param n_digits The number of digits that should be printed if x is a
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
force_or_cut <- function(x, n_digits) {
    return(vectorize_print(x, n_digits, force_or_cut_))
}

force_or_cut_ <- function(x, n_digits) {
    x <- as.numeric(x)
    if (x%%1==0) return(as.character(x))
    else return(force_decimals_(x, n_digits))
}

## An abstract function used to vectorize all number printing functions
vectorize_print <- function(x, n_digits, FUN) {
    x_ <- vapply(x, FUN, FUN.VALUE="character", n_digits)
    return(x_)
}
