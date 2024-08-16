

#' Generic function for \code{coreplot.pcss.core}
#'
#' @param x An object of class \code{pcss.core}.
#'
#' @param ...  Unused.
#'
#' @export
#' @keywords internal
#'
coreplot <- function(x, ...){
  UseMethod("coreplot")
}

#' Generic function for \code{contrib.pcss.core}
#'
#' @param x An object of class \code{pcss.core}.
#'
#' @param ...  Unused.
#'
#' @export
#' @keywords internal
#'
contrib <- function(x, ...){
  UseMethod("contrib")
}
