#' Plot Contribution or Loadings of Traits for each Dimension/Factor from
#' \code{pcss.core} Output
#'
#' \code{screeplot.pcss.core} generates bar plots of contributions or loadings
#' ("right singular vectors") of traits for each dimension/factor from the
#' output of \code{pcss.core}.
#'
#' @param An object of class \code{pcss.core}.
#' @param ndim The number of dimensions for which contribution or loadings of
#'   traits are to be plotted.
#' @param plot.loadings If \code{TRUE}, the loadings or "right singular vectors"
#'   are plotted instead of contributions. Default is \code{FALSE}.
#' @param use.sign If \code{TRUE}, contributions of variables are given the sign
#'   of their corresponding coordinates. Default is \code{TRUE}.
#' @param sort.value If \code{TRUE}, the bars are sorted according to their
#'   value.
#'
#' @return The contributions/loadings bar plot as a \code{ggplot} object.
#'
#' @import ggplot2
#' @importFrom tidyr pivot_longer
#' @export
#'
#' @examples
contrib.pcss.core <- function(x, ndim = NULL,
                              plot.loadings = FALSE,
                              use.sign = TRUE,
                              sort.value = TRUE) {
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

  if (plot.loadings) {
    pdata <- x$raw.out$svd$V

    rownames(pdata) <- rownames(x$raw.out$var$coord)
    colnames(pdata) <- colnames(x$raw.out$var$coord)

  } else {
    pdata <- x$raw.out$var$contrib

    if (use.sign) {
      pdata <- pdata * sign(x$raw.out$var$coord)
    }

  }

  pdata <- data.frame(pdata)

  if (!is.null(ndim)) {
    if (nrow(eigdf) > ndim) {
      pdata <- pdata[, 1:ndim]
    }
  }

  pdata$Trait <- rownames(pdata)

  pdata_long <- tidyr::pivot_longer(data = pdata, cols = !Trait,
                               names_to = "Dimension",
                               values_to = "Value")
  pdata_long$Dimension <- as.factor(as.integer(gsub("\\D", "",
                                                     pdata_long$Dimension)))
  levels(pdata_long$Dimension) <- paste("Dim", levels(pdata_long$Dimension))

  pdata_long$Group <- ifelse(pdata_long$Value >= 0, TRUE, FALSE)

  pdata_long <- pdata_long[rev(order(pdata_long$Value)), ]
  pdata_long$Trait_Dim <- paste(pdata_long$Trait,
                                pdata_long$Dimension, sep = "_")
  pdata_long$Trait_Dim <- factor(pdata_long$Trait_Dim,
                                 levels = rev(paste(pdata_long$Trait,
                                                    pdata_long$Dimension,
                                                    sep = "_")))

  if (sort.value) {

    contribg <-
      ggplot(pdata_long, aes(y = Trait_Dim, x = Value, fill = Group)) +
      geom_bar(stat = "identity", show.legend = FALSE,
               colour = "transparent") +
      facet_wrap(vars(Dimension), scales = "free_y") +
      scale_y_discrete(labels = function(x) gsub("_.+$", "", x)) +
      ylab("Trait") +
      theme_bw()

  } else {

    contribg <-
      ggplot(pdata_long, aes(y = Trait, x = Value, fill = Group)) +
      geom_bar(stat = "identity", show.legend = FALSE,
               colour = "transparent") +
      facet_wrap(vars(Dimension), scales = "free_y") +
      theme_bw()
  }

  if (any(pdata_long$Value < 0)) {

    contribg <- contribg +
      scale_fill_manual(values = c("firebrick2", "steelblue3")) +
      geom_vline(xintercept = 0)


  } else {

    contribg <- contribg +
      scale_fill_manual(values = c("gray10", "gray10")) +
      geom_vline(xintercept = 0)

  }

  return(contribg)

}



