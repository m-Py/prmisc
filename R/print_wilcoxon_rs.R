
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
#' @param effsize a character indicating which effect size should be
#'                reported, if any. Possible values are: 'r', 'rsqu'
#'                and 'd'. By default, no effect size will be reported.
#'                See details.
#' @param neg a logical indicating whether the effect size should
#'            be reported with a negative sign. Defaults to \code{FALSE}. 
#'            See details.
#' @param N an integer passing information about the total N of the
#'          sample. Needed when effect sizes should be calculated, but
#'          N cannot be inferred because neither of group1, group2 or
#'          groupvar have been provided. Should groups or a grouping
#'          variable have been provided, N must not be used.
#' @param decimals_eff an integer specifying how many digits the effect
#'                     size should be printed with, if an effect size
#'                     is desired. Defaults to 2. r and r squared will
#'                     be printed without a leading zero, d will be
#'                     printed with a leading zero because it is possible
#'                     for d to take values larger than one.
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
#' There are three options available for calculating an effect size via
#' the argument \code{effsize}: 
#' 
#' - the point biserial correlation r (\code{effsize = 'r'}), which 
#' is calculated by dividing Z-score by the square root of N. According
#' to Cohen (1988) a small, medium and large effect correspond to r = .1,
#' .3 and .5, respectively. Currently, \code{\link{print_wilcoxon_rs}} 
#' infers the Z-score from the p-value.
#' 
#' - r squared (identical to eta squared; \code{effsize = 'rsqu'}), 
#' which is the ratio of variability associated with an effect compared 
#' to the ratio of overall variance
#' 
#' - d (\code{effsize = 'd'}), which is calculated from r as follows:
#'  
#' \code{2*r/(sqrt(1-r^2))}
#' 
#' According to Cohen (1988), a small, medium and large effect correspond 
#' to r = .2, .5 and .8, respectively.
#' 
#' Information on the direction of the effect (as indicated in the sign 
#' of the Z-score) or in which order the groups have been compared is 
#' not included in the output of \code{\link{wilcox.test}}. Hence, only 
#' absolute effect sizes can be calculated. It is advised to report
#' absolute effect sizes unless there is a meaningful for the two groups
#' tested (e.g. Fritz et al, 2012). In some cases, it might be desired to
#' specify the direction of an effect by including the sign of the effect.
#' To that end, it is possible to print the negative effect size with 
#' \code{neg = TRUE}, but caution is advised: You should always check if 
#' the sign of the effect size you report is the correct one, especially 
#' in the case of one-sided testing.
#'
#' @examples 
#' 
#' data(iris)
#' dat <- subset(iris, Species %in% c("setosa", "versicolor"))
#' wc_iris <- wilcox.test(dat$Sepal.Length ~ dat$Species, 
#'                        correct = FALSE, paired = FALSE)
#' # include this call in Rmd inline code
#' print_wilcoxon_rs(wc_iris)
#' 
#' wc_iris2 <- wilcox.test(dat$Sepal.Width ~ dat$Species, 
#'                         correct = FALSE, paired = FALSE)
#'                         
#' print_wilcoxon_rs(wc_iris2, consistent = "min", 
#'                   group1 = dat$Sepal.Width[dat$Species == "setosa"], 
#'                   group2 = dat$Sepal.Width[dat$Species == "versicolor"])
#' 
#' print_wilcoxon_rs(wc_iris2, consistent = "max", 
#'                   groupvar = dat$Species)
#'
#' print_wilcoxon_rs(wc_iris2, consistent = "max", 
#'                   groupvar = dat$Species, effsize = "rsqu")
#'                   
#' @references
#' 
#' Cohen, J. (1988). Statistical power analysis for the behavioral sciences
#'     (2nd ed.). Hillsdale, NJ: Erlbaum.
#' 
#' Fritz, C. O., Morris, P. E., & Richler, J. J. (2012). Effect size 
#'     estimates: Current use, calculations, and interpretation. Journal 
#'     of Experimental Psychology: General, 141(1), 2-18. 
#'     http://dx.doi.org/10.1037/a0024338
#'
#' @author Juliane Tkotz \email{juliane.tkotz@@hhu.de}
#' @export
#'
print_wilcoxon_rs <- function(wc_object, decimals_p = 3, consistent = NULL,
                              group1 = NULL, group2 = NULL, groupvar = NULL, effsize = NULL,
                              neg = FALSE, N = NULL, decimals_eff = NULL) {
  
  # validate input and return decimals_eff should it not have been specified, but is needed
  decimals_eff <- val_input_wilcox(wc_object, decimals_p, consistent, group1, 
                                   group2, groupvar, effsize, neg, N, decimals_eff)
  
  ## Here the input is valid:
  ## (a) (group1, group2) and (groupvar) do not occur together
  ## (b) either one of (group1, group2) or (groupvar) or (wc_object) is provided
  ## (c) A warning is given if (group1, group2) or (groupvar) are provided together
  ##     with consistent
  
  if (!argument_exists(consistent)) consistent <- FALSE
  
  groupversion <- argument_exists(group1)
  groupvarversion <- argument_exists(groupvar)
  
  p <- format_p(wc_object$p.value, decimals_p)
  U <- wc_object$statistic
  
  if (groupversion == TRUE) {
    ngroup1 <- length(group1)
    ngroup2 <- length(group2)
  } else if (groupvarversion == TRUE) {
    ngroup1 <- table(groupvar)[table(groupvar) != 0][1]
    ngroup2 <- table(groupvar)[table(groupvar) != 0][2]
  }
  if (groupversion | groupvarversion) {
    U2 <- ngroup1 * ngroup2 - U
    U_min <- min(U, U2)
    U_max <- max(U, U2)
    if (consistent == "min") {
      U <- U_min
    } else if (consistent == "max") {
      U <- U_max
    }
  }
  
  U <- paste0("$U = ", U, "$")
  
  if (!argument_exists(effsize)) {
    return(paste(U, p, sep = ", "))
  }
  
  if (!argument_exists(N)) {
    N <- ngroup1 + ngroup2
  }
  
  effect <- calculate_effsize(wc_object, N, effsize, neg, decimals_eff)
  return(paste(U, p, effect, sep = ", "))
}

val_input_wilcox <- function(wc_object, decimals_p, consistent,
                             group1, group2, groupvar, effsize, neg, N, decimals_eff) {
  
  if (argument_exists(groupvar) && !argument_exists(consistent) && !argument_exists(effsize)) {
    warning("Argument groupvar was ignored because argument consistent and effsize were NULL")
  }
  if (argument_exists(group1) && !argument_exists(consistent) && !argument_exists(effsize)) {
    warning("Argument group1 was ignored because argument consistent and effsize were NULL")
  }
  if (argument_exists(group2) && !argument_exists(consistent) && !argument_exists(effsize)) {
    warning("Argument group2 was ignored because argument consistent and effsize were NULL")
  }
  
  # Input validation
  validate_input(wc_object, "wc_object", "htest")
  validate_input(decimals_p, "decimals_p", "numeric", 1, TRUE, TRUE)
  
  if (argument_exists(decimals_eff)) {
    if (!argument_exists(effsize)) {
      stop("decimals_eff can only be used with effsize")
    }
    
    validate_input(decimals_eff, "decimals_eff", "numeric", 1, TRUE, TRUE)
  }
  
  if (argument_exists(consistent)) {
    validate_input(consistent, "consistent", len = 1, input_set = c("min", "max"))
  }
  
  if (argument_exists(group1)) {
    validate_input(group1, "group1", "numeric")
    if (!argument_exists(group2)) {
      stop("group2 must not be NULL when group 1 is used")
    }
    if (argument_exists(groupvar)) {
      stop("groupvar must not be used with group1 or group2")
    }
  }
  
  if (argument_exists(group2)) {
    validate_input(group2, "group2", "numeric")
    if (!argument_exists(group1)) {
      stop("group1 must not be NULL when group 2 is used")
    }
    if (argument_exists(groupvar)) {
      stop("groupvar must not be used with group1 or group2")
    }
  }
  
  if (argument_exists(groupvar)) {
    validate_input(groupvar, "groupvar", "groupvariable", groupsize = 2)
    if (argument_exists(group1) | argument_exists(group2)) {
      stop("groupvar must not be used with group1 or group2")
    }
  }
  
  if (argument_exists(effsize)) {
    if (!argument_exists(group1) && !argument_exists(groupvar) && !argument_exists(N)) {
      stop("Information about sample size needs to be provided for effect size calculation via either N, group1 and group2 or groupvar")
    }
    
    if (!argument_exists(decimals_eff)) {
      decimals_eff <- 2
    }
    
    validate_input(effsize, "effsize", len = 1, input_set = c("r", "rsqu", "d"))
  }
  
  validate_input(neg, "neg", "logical", 1)
  
  if (neg == TRUE) {
    if (!argument_exists(effsize)) {
      stop("neg can only be used with effsize")
    }
    
    if (effsize == "rsqu") {
      stop("r squared cannot be negative")
    }
  }
  
  if (argument_exists(N)) {
    if(argument_exists(group1) | argument_exists(groupvar)) {
      stop("N must not be used when group1, group2 or groupvar are provided")
    }
    
    validate_input(N, "N", "numeric", 1, TRUE, TRUE)
  }
  
  return(decimals_eff)
  
}

calculate_effsize <- function(wc_object, N, effsize, neg, decimals_eff){
  if (wc_object$alternative == "two.sided") {
    Z <- abs(qnorm(wc_object$p.value/2))
  } else {
    Z <- abs(qnorm(wc_object$p.value))
  }
  
  r <- Z/sqrt(N)
  r2 <- r^2
  d <- 2*r/(sqrt(1 - r^2))
  
  if (neg == TRUE){
    r <- -r
    d <- -d
  }
  
  if (effsize == "r") {
    return(paste0("$r = ", decimals_only(r, decimals_eff), "$"))
  } else if (effsize == "rsqu") {
    return(paste0("$r^2 = ", decimals_only(r2, decimals_eff), "$"))
  } else if (effsize == "d") {
    return(paste0("$d = ", force_decimals(d, decimals_eff), "$"))
  }
}