
#' A function for input validation
#' 
#' @param obj The object that undergoes validation
#' @param argument_name A string indicating the name of the object 
#'   This name is used when an error is thrown so the user 
#'   is informed on the cause of the error.
#' @param class_string A character vector of legal classes
#' @param len Optional numeric vector for objects having a length
#'   (mostly for vectors).
#' @param gt0 Optional logical vector indicating if numeric input has 
#'   to be greater than 0.
#' @param must_be_integer Optional logical vector indicating if numeric
#'   input has to be integer.
#' 
#' @return NULL 
#' 
#' @noRd

validate_input <- function(obj, argument_name, class_string, len = NULL, 
                           gt0 = FALSE, must_be_integer = FALSE) {
  # Validate input for the `validate_input` function
  stopifnot(class(class_string) == "character")
  stopifnot(class(argument_name) == "character")
  if (!is.null(len)) {
    stopifnot(class(len) %in% c("numeric", "integer"))
    stopifnot(len >= 0)
  }
  stopifnot(class(gt0) == "logical")
  stopifnot(length(gt0) == 1)
  stopifnot(class(must_be_integer) == "logical")
  stopifnot(length(must_be_integer) == 1)
  
  ## - Check class of object
  correct_class <- class(obj) %in% class_string
  if (!correct_class) {
    stop(argument_name, " must be of class '", 
         paste(class_string, collapse = "' or '"), "'")
  }
  ## - Check length of input
  if (!is.null(len)) {
    if (length(obj) != len) {
      stop(argument_name, " must have length ", len)
    }
  }
  ## - Check if input must be greater than 0
  if (gt0 == TRUE && any(obj <= 0)) {
    stop(argument_name, " must be greater than 0")
  }
  ## - Check if input must be integer
  if (must_be_integer == TRUE && any(obj %% 1 != 0)) {
    stop(argument_name, " must be integer")
  }
  return(invisible(NULL))
}
