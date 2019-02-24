
#' A function for input validation
#' 
#' @param obj The object that undergoes validation
#' @param argument_name A string indicating the name of the object
#' @param class_string A character vector of legal classes
#' @param len Optional numeric vector for objects having a length
#'   (mostly vectors)
#' @param gt0 Optional logical vector indicating that a numeric vector
#'   have be greater than 0. Works for input vectors of length 1.
#' 
#' 
#' @noRd


validate_input <- function(obj, argument_name, class_string, len = NULL, 
                           gt0 = FALSE) {
  # Validate input for this function
  stopifnot(class(class_string) == "character")
  stopifnot(class(argument_name) == "character")
  if(!is.null(len)) {
    stopifnot(class(len) %in% c("numeric", "integer"))
    stopifnot(len >= 0)
  }
  stopifnot(class(gt0) == "logical")
  
  # Actual input validation
  correct_class <- class(obj) %in% class_string
  if (!correct_class) {
    stop(argument_name, " must be of class '", paste(class_string, collapse = "' or '"), "'")
  }
  if (!is.null(len)) {
    if (length(obj) != len) {
      stop(argument_name, " must have length ", len)
    }
    if (len == 1 && gt0 == TRUE && obj <= 0) {
      stop(argument_name, " must be greater than 0")
    }
  }
}
