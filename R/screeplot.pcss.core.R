#' Plot Eigen values as a Scree Plot from \code{pcss.core} Output
#'
#' \code{screeplot.pcss.core} generates a scree plot of eigen values from the
#' output of \code{pcss.core}.
#'
#' @param An object of class \code{pcss.core}.
#' @param ndim The number of eigen values to be plotted in the scree plot.
#' @param show.values If \code{TRUE}, the eigen values are shown in the plot as
#'   annotation labels. Default is \code{TRUE}.
#'
#' @return The scree plot as a \code{ggplot} object.
#'
#' @import ggplot2
#' @export
#'
#' @examples
screeplot.pcss.core <- function(x, ndim = NULL, show.values = TRUE) {

  # Checks ----

  # Check class of "x"
  if (!is(x, "pcss.core")) {
    stop('"x" is not of class "pcss.core".')
  }

  if (!is.null(ndim)) {
    # check if 'ndim' argument is integer vector of unit length
    if (!(is.integer(ndim) && length(ndim) == 1)) {
      stop('"ndim" should be a integer vector of unit length.')
    }

    # check if at least one dimension are plotted in ndim
    if (ndim < 1L) {
      stop('At least 1 dimension is to be specified in "ndim".')
    }

    # check if ndim is not greater than total dimensions
    total.ndim <- ncol(x$raw.out$svd$V)

    if (ndim > total.ndim) {
      warning('"ndim" is greater than the total number of dimensions.\n',
              paste('Using the total number of dimensions (',
                    total.ndim, ') as "ndim".', sep = ""))
      ndim <- total.ndim
    }

  }

  eigen.threshold <- x$eigen.threshold
  eig <- x$eigen$`Eigen value`

  eigen.threshold_label <- eigen.threshold
  if (nchar(eigen.threshold > 5)) {
    eigen.threshold_label <- round(eigen.threshold, 3)
  }

  K <- x$details["Number of eigen values selected", "Value"]
  K <- as.numeric(K)

  # Plot eigen values ----
  eigdf <- data.frame(sl = seq_along(eig), eig = eig)
  eigdf$gp <- ifelse(eigdf$sl <= K, 1, 0)
  eigdf$gp <- as.factor(eigdf$gp)

  if (!is.null(ndim)) {
    if (nrow(eigdf) > ndim) {
      eigdf <- eigdf[1:ndim, ]
    }
  }

  eigg <- ggplot(eigdf) +
    geom_segment(aes(x = sl, y = 0, xend = sl, yend = eig),
                 linewidth = 2, colour = "gray20") +
    geom_line(aes(x = sl, y = eig)) +
    geom_point(aes(x = sl, y = eig, colour = gp), pch = 18, size = 3,
               show.legend = FALSE) +
    scale_colour_manual(values = c("gray", "red")) +
    geom_hline(yintercept = eigen.threshold, linetype = 2) +
    geom_label(x = length(eig), y = eigen.threshold, colour = "red",
               label = eigen.threshold_label) +
    xlab("Factors") +
    ylab("Eigen value") +
    theme_bw()

  if (show.values) {
    eigg <- eigg +
      geom_label(aes(x = sl, y = eig, label = round(eig, 3)),
                 vjust = 1, hjust = -0.2) +
      scale_x_continuous(breaks = eigdf$sl,
                         limits = c(1, length(eig) + 0.5))
  }

  return(eigg)
}
