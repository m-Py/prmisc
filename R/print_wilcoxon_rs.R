

#' Print the results of a Wilcoxon rank sum test
#'
#' @param wc_object an object returned by `wilcox.test`
#' @param decimals_p how many decimals should be printed for the 
#'                   p-value (defaults to 3)
#' @param consistent a parameter determining for which group U
#'                   should be reported. Defaults to FALSE.
#'                   Can be set to "min" or "max". See details.
#' @param ngroup1 size of the first group
#' @param ngroup1 size of the second group
#' 
#' @details 
#' 
#' In order to calculate a Wilcoxon rank sum test, the argument 
#' \code{paired} in \code{\link{wilcox.test}} needs to be \code{FALSE}. Otherwise, a
#' Wilcoxon signed rank test will be computed instead and the
#' statistics printed by \code{\link{print_wilcoxon_rs}} will be misleading.
#' 
#' Note that the W calculated in \code{\link{wilcox.test}} that is used as
#' U in \code{\link{print_wilcoxon_rs}} will correspond to the U of the first
#' of the two groups compared. In the default method, this is the
#' vector assigned to x. In the formula method, this is the first
#' group as identified by the grouping variable. Some software,
#' like SPSS, consistently reports the smaller or larger U. If
#' you wish to mimic this, you can specify by setting the desired
#' behaviour via the \code{consistent} argument. Setting \code{consistent}
#' to "min" will print the smaller U, setting it to "max" will
#' print the larger U. In order to do so, you need to provide
#' the n of both groups via the arguments \code{ngroup1} and \code{ngroup2},
#' respectively. By default, \code{consistent} is \code{FALSE} and 
#' \code{\link{print_wilcoxon_rs}} will print U using W as provided by
#' \code{\link{wilcox.test}}.
#'
#' @examples 
#' 
#' data(iris)
#' head(iris)
#' dat <- subset(iris, 
#'               Species == c("setosa", "versicolor"))
#' wc_iris <- 
#'   wilcox.test(dat$Sepal.Length ~ dat$Species, 
#'               correct = FALSE, paired = FALSE)
#' # include this call in Rmd inline code
#' print_wilcoxon_rs(wc_iris)
#' 
#' wc_iris2 <- 
#'   wilcox.test(dat$Sepal.Length ~ dat$Species, 
#'               correct = FALSE, paired = FALSE)
#' # include this call in Rmd inline code
#' print_wilcoxon_rs(wc_iris2, consistent = "min", 
#'                   ngroup1 = 25, ngroup2 = 25)
#' 
#' # include this call in Rmd inline code
#' print_wilcoxon_rs(wc_iris2, consistent = "max", 
#'                   ngroup1 = 25, ngroup2 = 25)
#'
#' @author Juliane Tkotz \email{juliane.tkotz@@hhu.de}
#' @export
#'
print_wilcoxon_rs <- function(wc_object, decimals_p = 3, consistent = FALSE, 
                              ngroup1 = FALSE, ngroup2 = FALSE) {
  p <- format_p(wc_object$p.value, decimals_p)
  U1 <- wc_object$statistic
  
  if (consistent != FALSE) {
    
    # check if group is integer
    if(ngroup1 == FALSE | ngroup2 == FALSE) {
      stop("Please provide sample sizes.")
      
    } else if (ngroup1 %% 1 != 0 | ngroup2 %% 1 != 0) {
      stop("Group sizes must be integers.")
      
    } else {
      U2 <- ngroup1 * ngroup2 - U1
      U_min <- min(U1, U2)
      U_max <- max(U1, U2)
    }
  }
    
  if (consistent == "min") {
    U <- paste0("$U = ", U_min)
    
  } else if (consistent == "max") {
    U <- paste0("$U = ", U_max)
    
  } else if (consistent == FALSE) {
    U <- paste0("$U = ", U1)
    
  } else {
    stop("Consistent can either be FALSE, 'min' or 'max'.")
  }
  
  return(paste(U, p, sep = ", "))
}
