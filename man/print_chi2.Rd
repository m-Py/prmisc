% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/print_chi2.R
\name{print_chi2}
\alias{print_chi2}
\title{Print the results of a chi-square test}
\usage{
print_chi2(
  x,
  es = TRUE,
  correct = FALSE,
  decimals = 2,
  decimals_p = 3,
  italic_greek = TRUE
)
}
\arguments{
\item{x}{A contingency table (passed as \code{table} or \code{matrix}) or
an object of type "htest" returned by \code{\link{chisq.test}}. Can 
also handle objects returned by \code{\link[spgs]{chisq.unif.test}}
from the \code{spgs} package.}

\item{es}{Boolean. Should the phi coefficient be printed as a 
measure of effect size. See details.}

\item{correct}{Boolean. Apply a continuity correction? See
\code{\link{chisq.test}}. Only has an effect if the chi-square-test
is computed by this function, i.e., if \code{x} is a contingency 
table. The default value is \code{FALSE}.}

\item{decimals}{How many decimals should be printed}

\item{decimals_p}{How many decimals should be printed for the p-value
(defaults to 3)}

\item{italic_greek}{Should the greek letters (for chi and possily phi)
be printed in italic font. Defaults to \code{TRUE}. According to APA style
this should be \code{FALSE}.}
}
\value{
A string describing the results of the chi-square test to be
    printed in Rmarkdown documents.
}
\description{
Print the results of a chi-square test
}
\details{
The argument \code{es} only has an effect if \code{x} is passed as a 2x2
contingency table. In this case, the phi coefficient is computed as 
a measure of effect size (see Cohen, 1988, page 223).
}
\examples{

# Pass a matrix
x <- matrix(c(12, 5, 7, 7), ncol = 2)
print_chi2(x) # does not use continuity correction by default
print_chi2(x, correct = TRUE) # uses continuity correction

# Pass a table
tab <- table(rbinom(150, 1, 0.5), rbinom(150, 1, 0.1))
print_chi2(tab, correct = FALSE)

# Pass a chi-squared test object
print_chi2(chisq.test(tab, correct = FALSE))

# Use non italic chi symbol
print_chi2(tab, italic_greek = FALSE)

}
\references{
Cohen, J. (1988). Statistical power analysis for the behavioral sciences
    (2nd ed.). Hillsale, NJ: Lawrence Erlbaum.
}
\author{
Martin Papenberg \email{martin.papenberg@hhu.de}
}
