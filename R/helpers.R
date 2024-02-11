#' Helper function to assign xlab and ylab to variable name if labels not supplied for plotting function
#'
#' This function is used internally by plotting functions to assign xlab and ylab to the variable name if labels are not supplied.
#'
#' @param lab The label to check.
#' @param var The variable name to use as the label if \code{lab} is NULL.
#' @return The label to use, either \code{lab} if not NULL, or the variable name of \code{var}.
null_check <- function(lab, var) {
  if (is.null(lab))
    return(ensym(var))
  else
    return(lab)
}
