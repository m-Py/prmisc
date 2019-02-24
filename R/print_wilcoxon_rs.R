

#' Print the results of a Wilcoxon rank sum test (Mann-Whitney-U test)
#'
#' @param wc_object an object returned by \code{\link{wilcox.test}}
#' @param decimals_p how many decimals should be printed for the 
#'                   p-value (defaults to 3)
#' @param consistent a parameter determining for which group the test
#'                   statistic U should be reported. Defaults to FALSE.
#'                   Can be set to "min" or "max". See details.
#' @param group1 a vector containing the cases of the first group
#' @param group2 a vector containing the cases of the second group
#' 
#' @details 
#' 
#' In order to calculate a Wilcoxon rank sum test, the argument 
#' \code{paired} in \code{\link{wilcox.test}} needs to be \code{FALSE}. 
#' Otherwise, a Wilcoxon signed rank test will be computed instead and 
#' the statistics printed by \code{\link{print_wilcoxon_rs}} will be 
#' misleading.
#' 
#' Note that the test statistic W calculated in \code{\link{wilcox.test}} 
#' that is printed as test statistic U in \code{\link{print_wilcoxon_rs}} 
#' will correspond to the U of the first of the two groups compared in
#' \code{\link{wilcox.test}}. In the default method of 
#' \code{\link{wilcox.test}}, this is the vector assigned to x. In the 
#' formula method, this is the first group as identified by the grouping 
#' variable. Some software, like SPSS, consistently reports the smaller or 
#' larger U. If you wish to mimic this, you can specify the desired
#' behaviour by providing the \code{consistent} argument. Setting 
#' \code{consistent} to "min" will print the smaller of the two U, setting 
#' it to "max" will print the larger U. In order to do so, you need to provide 
#' the n for both groups, which \code{\link{print_wilcoxon_rs}} will calculate
#' when you pass the data of both groups to the arguments \code{group1} and 
#' \code{group2}, respectively. Any vector with the length of the corresponding 
#' group size will produce the desired result.
#' By default, \code{consistent} is \code{FALSE} and 
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
#'   wilcox.test(dat$Sepal.Width ~ dat$Species, 
#'               correct = FALSE, paired = FALSE)
#' # include this call in Rmd inline code
#' print_wilcoxon_rs(wc_iris2, consistent = "min", 
#'                   group1 = dat$Sepal.Width[dat$Species == "setosa"], 
#'                   group2 = dat$Sepal.Width[dat$Species == "versicolor"])
#' 
#' # include this call in Rmd inline code
#' print_wilcoxon_rs(wc_iris2, consistent = "max", 
#'                   group1 = dat$Sepal.Width[dat$Species == "setosa"], 
#'                   group2 = dat$Sepal.Width[dat$Species == "versicolor"])
#'
#' @author Juliane Tkotz \email{juliane.tkotz@@hhu.de}
#' @export
#'
print_wilcoxon_rs <- function(wc_object, decimals_p = 3, 
                              consistent = FALSE, 
                              group1 = NULL, group2 = NULL) {
  p <- format_p(wc_object$p.value, decimals_p)
  U1 <- wc_object$statistic
  
  if (consistent != FALSE) {
    
    # check if groups are provided in required format
    if(length(group1) == 0 | length(group2) == 0) {
      stop("Two vectors containing group data required if consistent != FALSE.")
      
    } else if (!is.atomic(group1) | !is.atomic(group2)) {
      stop("Group data must be provided as vectors.")
      
    } else {
      U2 <- length(group1) * length(group2) - U1
      U_min <- min(U1, U2)
      U_max <- max(U1, U2)
    }
  }
    
  if (consistent == "min") {
    U <- paste0("$U = $", U_min)
    
  } else if (consistent == "max") {
    U <- paste0("$U = $", U_max)
    
  } else if (consistent == FALSE) {
    U <- paste0("$U = $", U1)
    
  } else {
    stop("Consistent can either be FALSE, 'min' or 'max'.")
  }
  
  return(paste(U, p, sep = ", "))
}
