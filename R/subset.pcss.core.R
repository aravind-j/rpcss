#' Fetch the names of individuals/genotypes in the core set generated from
#' \code{pcss.core} Output
#'
#' \code{subset.pcss.core} returns names of individuals/genotypes in the core
#' collection from \code{pcss.core} Output.
#'
#' Use \code{"size"} to return names of individuals/genotypes in the core
#' collection according to the threshold \code{size} criterion or use
#' \code{"variance"} to return names according to the variability threshold
#' criterion or use  \code{"logistic"} to return names according to inflection
#' point of rate of progress of cumulative variability retained identified by
#' logistic regression.
#'
#' @param x An object of class \code{pcss.core}.
#' @param criterion The core collection generation criterion. Either
#'   \code{"size"}, \code{"variance"}, or \code{"logistic"}. See
#'   \strong{Details}.
#'
#' @return The names of individuals/genotypes in the core collection as a
#'   character vector.
#' @export
#'
#' @examples
subset.pcss.core <- function(x,
                             criterion = c("size", "variance", "logistic")) {

  # Checks ----

  # Check class of "x"
  if (!is(x, "pcss.core")) {
    stop('"x" is not of class "pcss.core".')
  }

  criterion <- match.arg(criterion)

  gssdf <- x$variability.ret

  # By size specified ----

  if (criterion == "size") {
    size.sel <-
      x$cores.info[x$cores.info$Method == "By size specified", ]$Size

    subset <- gssdf[1:size.sel, ]$Id
  }

  # By threshold variance ----

  if (criterion == "variance") {
    var.sel <-
      x$cores.info[x$cores.info$Method == "By threshold variance", ]$Size

    subset <- gssdf[1:size.sel, ]$Id
  }

  # With logistic regression ----

  if (criterion == "logistic") {
    reg.sel <-
      x$cores.info[x$cores.info$Method == "By logistic regression", ]$Size

    subset <- gssdf[1:size.sel, ]$Id
  }

  return(subset)

}
