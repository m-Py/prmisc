

#' Print the results of a Wilcoxon rank sum test (Mann-Whitney-U test)
#'
#' @param wc_object an object returned by \code{\link{wilcox.test}}
#' @param decimals_p how many decimals should be printed for the 
#'                   p-value (defaults to 3)
#' @param consistent an optional parameter determining for which group 
#'                   the test statistic U should be reported. Can be 
#'                   set to 'min' or 'max'. See details.
#' @param group1 a vector containing the cases of the first group
#' @param group2 a vector containing the cases of the second group
#' @param groupvar a vector containing a grouping variable
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
#' \code{consistent} to 'min' will print the smaller of the two U, setting 
#' it to 'max' will print the larger U. In order to do so, you need to 
#' provide the n for both groups. 
#' 
#' You can either do that by passing the data of both groups to the 
#' arguments \code{group1} and \code{group2}, respectively. From those, 
#' \code{\link{print_wilcoxon_rs}} will calculate the group sizes. Any 
#' vector with the length of the corresponding group size will produce 
#' the desired result. This is the recommended option if your 
#' \code{wc_object} has been created using the default version of 
#' \code{\link{wilcox.test}}, i.e. if groups were passed as \code{x} and 
#' \code{y}. The order in which you pass \code{group1} and \code{group2} 
#' to \code{\link{print_wilcoxon_rs}} does not have to correspond to the 
#' order in which \code{x} and \code{y} were passed to 
#' \code{\link{wilcox.test}}.
#' 
#' Alternatively, you can pass a grouping variable to the argument 
#' \code{groupvar}. From this, \code{\link{print_wilcoxon_rs}} will 
#' calculate group sizes. This is the recommended option if your 
#' \code{wc_object} has been created using the formula version of 
#' \code{\link{wilcox.test}}, i.e. if a grouping variable was passed after 
#' the \code{~}. To prevent mistakes, you can either use \code{group1} and 
#' \code{group2} \bold{or} \code{groupvar}.
#' 
#' By default, when \code{consistent} is not provided, 
#' \code{\link{print_wilcoxon_rs}} will print U using W as taken from
#' \code{\link{wilcox.test}}.
#'
#' @examples 
#' 
#' data(iris)
#' head(iris)
#' dat <- subset(iris, 
#'               Species == c("setosa", "versicolor"))
#' wc_iris <- wilcox.test(dat$Sepal.Length ~ dat$Species, 
#'                        correct = FALSE, paired = FALSE)
#' # include this call in Rmd inline code
#' print_wilcoxon_rs(wc_iris)
#' 
#' wc_iris2 <- wilcox.test(dat$Sepal.Width ~ dat$Species, 
#'                         correct = FALSE, paired = FALSE)
#' # include this call in Rmd inline code
#' print_wilcoxon_rs(wc_iris2, consistent = "min", 
#'                   group1 = dat$Sepal.Width[dat$Species == "setosa"], 
#'                   group2 = dat$Sepal.Width[dat$Species == "versicolor"])
#' 
#' # include this call in Rmd inline code
#' print_wilcoxon_rs(wc_iris2, consistent = "max", 
#'                   groupvar = dat$Species)
#'
#' @author Juliane Tkotz \email{juliane.tkotz@@hhu.de}
#'
print_wilcoxon_rs <- function(wc_object, decimals_p = 3, consistent = NULL, 
                              group1 = NULL, group2 = NULL, groupvar = NULL) {
  # Run default version when group1, group2 or groupvar are not provided
  groupversion <- FALSE
  groupvarversion <- FALSE
  
  
  # Input validation
  validate_input(wc_object, "wc_object", "htest")
  validate_input(decimals_p, "decimals_p", "numeric", 1, TRUE, TRUE)
  
  if (!is.null(consistent)) {
  validate_input2(consistent, "consistent", c("min", "max"))
  }
  
  if (!is.null(group1)) {
    validate_input(group1, "group1", "numeric")
    
    if(is.null(group2)){
      stop("group2 must not be NULL when group 1 is used")
    }
    
    if(!is.null(groupvar)){
      stop("groupvar must not be used with group1 or group2")
    }
    
    groupversion <-  TRUE
  }
  
  if (!is.null(group2)) {
    validate_input(group2, "group2", "numeric")
    
    if(is.null(group1)){
      stop("group1 must not be NULL when group 2 is used")
    }
    
    if(!is.null(groupvar)){
      stop("groupvar must not be used with group1 or group2")
    }
  }
  
  if (!is.null(groupvar)) {
    validate_input(groupvar, "groupvar", "groupvariable", groupsize = 2)
    
    if(!is.null(group1) | !is.null(group2)){
      stop("groupvar must not be used with group1 or group2")
    }
    
    groupvarversion <- TRUE
  }
  
  
  p <- format_p(wc_object$p.value, decimals_p)
  U1 <- wc_object$statistic
  
  
  # check if an argument for consistent is provided
  if (!is.null(consistent)) {
    # groupversion
    if (groupversion == TRUE) {
      ngroup1 <- length(group1)
      ngroup2 <- length(group2)
    }
    
    # groupvarversion
    if (groupvarversion == TRUE) {
        ngroup1 <- table(groupvar)[table(groupvar) != 0][1] 
        ngroup2 <- table(groupvar)[table(groupvar) != 0][2]
        }
    
    U2 <- ngroup1 * ngroup2 - U1
    U_min <- min(U1, U2)
    U_max <- max(U1, U2)
    
    if (consistent == "min") {
      U <- paste0("$U = ", U_min, "$")
    } 
    
    if (consistent == "max") {
      U <- paste0("$U = ", U_max, "$")
    }
    
    } else {
    U <- paste0("$U = ", U1, "$")
    }
  
  return(paste(U, p, sep = ", "))
}
