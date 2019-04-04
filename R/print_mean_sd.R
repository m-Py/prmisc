
#' Print mean and standard deviation
#'
#' @param x a vector of the sample the statistics should be printed for.
#' @param decimals_M how many decimals should be printed for the 
#'                   mean (defaults to 2).
#' @param decimals_SD how many decimals should be printed for the 
#'                    standard deviation (defaults to 2).
#' @param parentheses logical indicating if the statistics should be
#'                    wrapped in parentheses or not (defaults to 
#'                    \code{TRUE}). Will be ignored if \code{short} is
#'                    \code{TRUE}.
#' @param short logical indicating if a short version without the letters
#'              should be printed. Argument \code{parantheses} is ignored
#'              in this case. Defaults to \code{FALSE}. See details.
#' @param na.rm logical indicating whether missing values should be
#'              ignored or not. Defaults to \code{FALSE}.
#'                
#' @details 
#' 
#' The following formatting options are available:
#' 
#' - When \code{parentheses} is \code{TRUE}: 
#' \eqn{(M = XX, SD = XX)}
#' 
#' - When \code{parentheses} is \code{FALSE}: 
#' \eqn{M = XX, SD = XX}
#' 
#' - When \code{short} is \code{TRUE}: \eqn{XX (XX)}
#'
#' @examples 
#' 
#' print_mean_sd(rnorm(100, 0, 1))
#' 
#' print_mean_sd(1:20, decimals_M = 0, decimals_SD = 3)
#' 
#' print_mean_sd(c(2, 10, 12.5, 3), parentheses = FALSE)
#'
#' @author Juliane Tkotz \email{juliane.tkotz@@hhu.de}
#' @export
#'
print_mean_sd <- function(x, decimals_M = 2, decimals_SD = 2, parentheses = TRUE,
                          short = FALSE, na.rm = FALSE) {
  
  # validate input
  validate_input(x, "x", "numeric")
  validate_input(decimals_M, "decimals_M", "numeric", 1, must_be_integer = TRUE)
  validate_input(decimals_SD, "decimals_SD", "numeric", 1, must_be_integer = TRUE)
  validate_input(parentheses, "parentheses", "logical", 1)
  validate_input(short, "short", "logical", 1)
  validate_input(na.rm, "na.rm", "logical", 1)
  
  M <- force_decimals(mean(x, na.rm = na.rm), decimals_M)
  
  SD <- force_decimals(sd(x, na.rm = na.rm), decimals_SD)
  
  if (short == TRUE) {
    return(paste0("$", M, "$", " $(", SD, ")$"))
  }
  
  if (parentheses == TRUE) {
    return(paste0("$(M = ", M, ", SD = ", SD, ")$"))
  } else {
    return(paste0("$M = ", M, ", SD = ", SD, "$"))
  }
}
