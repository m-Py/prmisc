
#' Print the results of an `afex` ANOVA object
#'
#' @param afex_object An object returned by one of afex's ANOVA functions
#' @param font Should the effect size symbol eta be printed in "italic"
#'     or "nonitalic". See details.  
#' @param decimals How many decimals should be printed for F values
#'     and eta-squared. Defaults to 2.
#' @param decimals_p How many decimals should be printed for p-values.
#'     Defaults to 3.
#' 
#' @details 
#' 
#' To use this function, you have to install afex and use afex to compute
#' an ANOVA object; pass this object as the first argument.
#'  
#' According to APA style, the eta symbol should be printed non-italic,
#' but the standard Latex \\eta symbol is italic. To print a 
#' non-italic eta, use font = "nonitalic". However, this option 
#' requires that you load the package `upgreek` in the YAML header of 
#' your R markdown document. To this end, use the following: 
#' 
#' header-includes:
#'   -\\usepackage{upgreek}
#'   
#' @examples 
#' 
#' library("afex")
#' # see ?aov_ez
#' data(md_12.1)
#' aov_results <- aov_ez("id", "rt", md_12.1, within = c("angle", "noise"))
#' print_anova(aov_results)
#' # Print nonitalic eta, which is required according to APA guidelines
#' print_anova(aov_results, font = "nonitalic")
#' # Example using other (or no) effect size index
#' pes <- aov_ez("id", "rt", md_12.1, within = c("angle", "noise"), 
#'               anova_table = list(es = "pes"))
#' print_anova(pes)
#' noes <- aov_ez("id", "rt", md_12.1, within = c("angle", "noise"), 
#'                anova_table = list(es = "none"))
#' print_anova(noes)
#'
#' @author Martin Papenberg \email{martin.papenberg@@hhu.de}
#' @export
#'

print_anova <- function(afex_object, font = "italic",
                        decimals = 2, decimals_p = 3) {
  rows <- rownames(afex_object$anova)
  return_list <- list()
  for (i in seq_along(rows)) {
    ## name the elements of the returned list by effect
    return_list[[rows[i]]] <- print_anova_(afex_object, i, font, decimals, decimals_p)
  }
  return(return_list)
}
  

print_anova_ <- function(afex_object, row, font = "nonitalic",
                        decimals = 2, decimals_p = 3) {
  
  aov.table <- afex_object$anova_table # contains the relevant values
  
  ## Set symbol for effect size
  cols <- names(aov.table)
  has_effect_size <- length(cols) == 6 # maybe afex object does not have effect size
  if (has_effect_size) 
    es <- cols[5]
  else 
    es <- ""
  
  if (es == "pes") es.symbol <- "p"
  else if (es == "ges") es.symbol <- "G"
  else es.symbol <- "" # if there was no eta-squared in the object

  # p-value
  p_value <- aov.table[row, "Pr(>F)"]
  p <- format_p(p_value, decimals_p)
    
  # F-value
  F <- paste0("$F(", force_or_cut(aov.table[row,"num Df"], decimals), "$, $",
              force_or_cut(aov.table[row,"den Df"], decimals), ") = ",
              force_decimals(aov.table[row,"F"], decimals), "$")
  
  # Print eta^2; either according to APA-style nonitalic (using
  # font="nonitalic"), or using font = "italic"; font = "nonitalic"
  # requires latex package \upgreek (for \upeta)
  if (font == "nonitalic") {
    eta_symbol <- paste0("$\\upeta_\\mathrm{", es.symbol ,"}^2 = ")
  } else if (font == "italic") {
    eta_symbol <- paste0("$\\eta_", es.symbol, "^2 = ")
  } else {
    stop("error in function print_anova: argument 'font' must be
         'nonitalic' or 'italic'")
  }
  eta <- paste0(eta_symbol, decimals_only(aov.table[row,es], decimals), "$")
  ret <- paste(F, p, eta, sep = ", ")
  if (!has_effect_size)
    ret <- paste(F, p, sep = ", ")
  return(ret)
}
