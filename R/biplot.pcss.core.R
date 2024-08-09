#' Generate Biplots from \code{pcss.core} Output
#'
#' \code{biplot.pcss.core} generates biplots of scores of genotypes with or
#' without vectors for traits from the output of \code{pcss.core}.
#'
#' @param An object of class \code{pcss.core}.
#' @param ndim The number of dimensions for which biplots have to plotted.
#' @param show.traits Which kind of the traits to be shown in the biplot. Either
#'   "all", "none", "quantitative" or "qualitative".
#' @param trait.scale A scale factor to be applied to trait coordinates plotted
#'   in biplot.
#' @param point.alpha Alpha transparency value for biplot points.
#' @param segment.alpha Alpha transparency value for biplot segments.
#'
#' @return A list of biplots as \code{ggplot} objects.
#'
#' @import ggplot2
#' @importFrom dplyr bind_rows
#' @importFrom ggrepel geom_label_repel
#' @export
#'
#' @examples
biplot.pcss.core <- function(x,
                             ndim = 3L, # at least 2
                             show.traits = c("all", "none",
                                             "quantitative", "qualitative"),
                             trait.scale = 1,
                             point.alpha = 0.8,
                             segment.alpha = 0.8) {


  # Checks ----

  # Check class of "x"
  if (!is(x, "pcss.core")) {
    stop('"x" is not of class "pcss.core".')
  }

  # check if 'ndim' argument is integer vector of unit length
  if (!(is.integer(ndim) && length(ndim) == 1)) {
    stop('"ndim" should be a integer vector of unit length.')
  }

  # check if at least two dimensions are plotted in ndim
  if (ndim < 2L) {
    stop('At least 2 dimensions are to be specified in "ndim".')
  }

  qualitative <- attributes(x)$qual
  quantitative <- attributes(x)$quant

  # check if ndim is not greater than total dimensions
  if (is.null(qualitative) & !is.null(quantitative)) {
    total.ndim <- length(quantitative)
  }

  if (!is.null(qualitative) & is.null(quantitative)) {
    total.ndim <- sum(unlist(lapply(data[, c(qualitative)],
                                    function(x) length(levels(x))))) -
      length(qualitative)
  }

  if (is.null(qualitative) & is.null(quantitative)) {
    total.ndim <- length(quantitative) + length(qualitative)
  }

  if (ndim > total.ndim) {
    warning('"ndim" is greater than the total number of dimensions.\n',
            paste('Using the total number of dimensions (',
                  total.ndim, ') as "ndim".', sep = ""))
    ndim <- total.ndim
  }

  # check show.traits argument
  show.traits <- match.arg(show.traits)

  # check trait.scale argument is numeric vector of unit length
  if (!(is.numeric(trait.scale) && length(trait.scale) == 1)) {
    stop('"trait.scale" should be a numeric vector of unit length.')
  }

  # check point.alpha argument is numeric vector of unit length
  if (!(is.numeric(point.alpha) && length(point.alpha) == 1)) {
    stop('"point.alpha" should be a numeric vector of unit length.')
  }

  # check if 'point.alpha' is a value between 0 and 1
  if (point.alpha <= 0 | point.alpha >= 1) {
    stop('"point.alpha" should be a value between 0 and 1.')
  }

  # check segment.alpha argument is numeric vector of unit length
  if (!(is.numeric(segment.alpha) && length(segment.alpha) == 1)) {
    stop('"segment.alpha" should be a numeric vector of unit length.')
  }

  # check if 'segment.alpha' is a value between 0 and 1
  if (segment.alpha <= 0 | segment.alpha >= 1) {
    stop('"segment.alpha" should be a value between 0 and 1.')
  }

  method <- attr(x, "method")

  # Get coordinates for biplot ----

  if (method == "PCA") {

    pca_out <- x$raw.out

    ## Coordinates for biplot
    ind_coord <- pca_out$ind$coord
    ind_coord <- ind_coord[, 1:ndim]
    colnames(ind_coord) <- gsub("Dim.", "Dim ", colnames(ind_coord))

    quant_coord <- pca_out$var$coord
    quant_coord <- quant_coord[, 1:ndim]
    colnames(quant_coord) <- gsub("Dim.", "Dim ", colnames(quant_coord))
    quant_coord <- quant_coord * trait.scale
  }

  if (method == "MCA") {

    mca_out <- x$raw.out

    ## Coordinates for biplot
    ind_coord <- mca_out$ind$coord
    ind_coord <- ind_coord[, 1:ndim]
    colnames(ind_coord) <- gsub("Dim.", "Dim ", colnames(ind_coord))

    qual_coord <- mca_out$var$coord
    qual_coord <- qual_coord[, 1:ndim]
    colnames(qual_coord) <- gsub("Dim.", "Dim ", colnames(qual_coord))
    qual_coord <- qual_coord * trait.scale
  }

  if (method == "FAMD") {

    famd_out <- x$raw.out

    ## Coordinates for biplot
    ind_coord <- famd_out$ind$coord
    ind_coord <- ind_coord[, 1:ndim]
    colnames(ind_coord) <- gsub("Dim.", "Dim ", colnames(ind_coord))

    quant_coord <- famd_out$quanti.var$coord
    quant_coord <- quant_coord[, 1:ndim]
    colnames(quant_coord) <- gsub("Dim.", "Dim ", colnames(quant_coord))
    quant_coord <- quant_coord * trait.scale

    qual_coord <- famd_out$quali.var$coord
    qual_coord <- qual_coord[, 1:ndim]
    colnames(qual_coord) <- gsub("Dim.", "Dim ", colnames(qual_coord))
    qual_coord <- qual_coord * trait.scale

    qual_levels <- lapply(data[, qualitative],
                          function(x) data.frame(qual_levels = levels(x)))
    qual_levels <- dplyr::bind_rows(qual_levels, .id = "qual")

    if (any(qual_levels$qual_levels != rownames(qual_coord))) {
      warning('Mismatch in levels of qualitative traits and ',
              'the names of qualitative trait level coordinates.')
    } else {
      rownames(qual_coord) <- paste(qual_levels$qual,
                                    qual_levels$qual_levels, sep = "_")
    }
  }

  # Plot Biplot ----

  imp <- x$eigen

  biplot_comb <- data.frame(t(combn(x = colnames(ind_coord), m = 2)))
  biplot_comb$label <- paste(biplot_comb$X1, biplot_comb$X2, sep = " vs. ")

  biplot_list <- vector("list", length = nrow(biplot_comb))
  names(biplot_list) <- biplot_comb$label

  for (i in seq_along(biplot_list)) {

    xlb <- paste(biplot_comb[i, 1], " (",
                 round(imp[biplot_comb[i, 1], ]$`Percentage of variance`, 2),
                 "% explained variance)", sep = "")
    ylb <- paste(biplot_comb[i, 2], " (",
                 round(imp[biplot_comb[i, 2], ]$`Percentage of variance`, 2),
                 "% explained variance)", sep = "")

    bipg <- ggplot(data = ind_coord,
                   aes(x = .data[[biplot_comb[i, 1]]],
                       y = .data[[biplot_comb[i, 2]]])) +
      geom_vline(xintercept = 0, linetype = 2, colour = "gray20") +
      geom_hline(yintercept = 0, linetype = 2, colour = "gray20") +
      geom_point(alpha = point.alpha) +
      xlab(label = xlb) +
      ylab(label = ylb) +
      theme_bw()

    if (show.traits == "all" | show.traits == "quantitative") {

      if ((is.null(qualitative) & !is.null(quantitative)) |
          (!is.null(qualitative) & !is.null(quantitative))) {

        bipg <- bipg +
          geom_segment(data = quant_coord,
                       aes(x = 0, y = 0,
                           xend = .data[[biplot_comb[i, 1]]],
                           yend = .data[[biplot_comb[i, 2]]]),
                       arrow = arrow(length = unit(0.2, "cm")),
                       color = "#619CFF", alpha = segment.alpha) +
          geom_label_repel(data = quant_coord,
                           aes(x = .data[[biplot_comb[i, 1]]],
                               y = .data[[biplot_comb[i, 2]]],
                               label = rownames(quant_coord)),
                           # vjust = -0.5,
                           fill = "#619CFF", colour = "white",
                           alpha = segment.alpha)
      }
    }

    if (show.traits == "all" | show.traits == "qualitative") {

      if ((!is.null(qualitative) & is.null(quantitative)) |
          (!is.null(qualitative) & !is.null(quantitative))) {

        bipg <- bipg +
          geom_segment(data = qual_coord,
                       aes(x = 0, y = 0,
                           xend = .data[[biplot_comb[i, 1]]],
                           yend = .data[[biplot_comb[i, 2]]]),
                       arrow = arrow(length = unit(0.2, "cm")),
                       color = "#00BA38", alpha = segment.alpha) +
          geom_label_repel(data = qual_coord,
                           aes(x = .data[[biplot_comb[i, 1]]],
                               y = .data[[biplot_comb[i, 2]]],
                               label = rownames(qual_coord)),
                           # vjust = -0.5,
                           fill = "#00BA38", colour = "white",
                           alpha = segment.alpha)
      }
    }

    biplot_list[[i]] <- bipg

    rm(bipg, xlb, ylb)

  }

  # patchwork::wrap_plots(biplot_list)

  return(biplot_list)

}
