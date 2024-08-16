#' Prints summary of \code{pcss.core} object
#'
#' \code{pcss.core} prints to console the summary of an object of class
#' \code{pcss.core} including the dimensionality reduction method used, the
#' basic details including parameters and the information on the core sets that
#' can be constituted.
#'
#' @param x An object of class \code{pcss.core}.
#' @param ... Unused.
#' @seealso \code{\link[rpcss]{pcss.core}}
#'
#' @export
print.pcss.core <- function(x, ...) {

  cat("\nMethod\n")
  cat("========================\n")

  print(attr(x, "method"))

  cat("\nDetails\n")
  cat("========================\n")

  print(x$details)

  cat("\nCore sets\n")
  cat("=========================\n")

  print(x$cores.info)

}
