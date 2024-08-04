
#' Principal Component Scoring to Generate Core collections
#'
#' Generate a Core Collection with Principal Component Scoring Strategy (PCSS)
#' \insertCite{noirot_principal_1996,noirot_method_2003}{rpcss} using
#' qualitative and/or quantitative trait data.
#'
#' For qualitative traits, dimensionality reduction is performed by Principal
#' Component Analysis (PCA). Similarly for quantitative traits, dimensionality
#' reduction is done via Correspondence Analysis (CA). For cases where both
#' quantitative and qualitative traits are available, dimensionality reduction
#' is achieved by using the Factor Analysis for Mixed Data (FAMD) method.
#'
#' @param data The data as a data frame object. The data frame should possess
#'   one row per individual and columns with the individual names and multiple
#'   trait/character data.
#' @param names Name of column with the individual names as a character string.
#' @param quantitative Name of columns with the quantitative traits as a
#'   character vector.
#' @param qualitative Name of columns with the qualitative traits as a character
#'   vector.
#' @param eigen.threshold The lower limit of the eigen value of factors to be
#'   included in the estimation. The default value is the average of all the
#'   eigen values.
#' @param size The desired core set size proportion.
#' @param var.threshold The desired percentage of total variabilty to be
#'   retained in the core set.
#' @param screeplot.ndim The number of eigen values to be plotted in the scree
#'   plot.
#' @param biplot.ndim The number of dimensions for which biplots have to
#'   plotted.
#' @param biplot.trait.scale A scale factor to be applied to trait coordinates
#'   plotted in biplot.
#' @param biplot.point.alpha Alpha transparency value for biplot points.
#' @param biplot.segment.alpha Alpha transparency value for biplot segments.
#'
#' @return A list of class \code{pcss.core} with the following components.
#'   \item{details}{The details of the core set generaton process.}
#'   \item{raw.out}{The original output of \code{\link[FactoMineR]{PCA}},
#'   \code{\link[FactoMineR]{CA}} and \code{\link[FactoMineR]{FAMD}} functions
#'   of \code{\link[FactoMineR]{FactoMineR}}} \item{eigen}{A vector of eigen
#'   values.} \item{rotation}{A matrix of rotation values or loadings.}
#'   \item{scores}{A matrix of scores from PCA, CA or FAMD.}
#'   \item{variability.ret}{A data frame of genotypes ordered by variability
#'   retained.} \item{scree.plot}{The scree plot as a \code{ggplot} object.}
#'   \item{biplot}{A list of biplot \code{ggplot} objects.} \item{cores.info}{A
#'   data frame of core set size and percentage variablity retained according to
#'   the method used.} \item{core.plots}{A list of plots of cumulative
#'   variability retained by genotypes as \code{ggplot} objects. The core set
#'   size and corresponding percentage variability retained are highlighted
#'   according to the method used.}
#'
#' @seealso \code{\link[FactoMineR]{PCA}}, \code{\link[FactoMineR]{CA}} and
#'   \code{\link[FactoMineR]{FAMD}}
#'
#' @import ggplot2
#' @import gslnls
#' @importFrom ggrepel geom_text_repel
#' @importFrom FactoMineR PCA CA FAMD
#' @importFrom Rdpack reprompt
#' @export
#'
#' @examples
pcss.core <- function(data, names, quantitative, qualitative,
                      eigen.threshold = NULL,
                      size = 0.2, var.threshold = 0.75,
                      screeplot.ndim = NULL,
                      biplot.ndim = 3, # at least 2
                      biplot.trait.scale = 1,
                      biplot.point.alpha = 0.8,
                      biplot.segment.alpha = 0.8) {

  # Checks ----

  if (missing(quantitative)) {
    quantitative <- NULL
  }

  if (missing(qualitative)) {
    qualitative <- NULL
  }

  if (length(c(quantitative, qualitative)) == 1) {
    stop("Only one trait specified")
  }

  # check if 'data' is a data frame object
  if (!is.data.frame(data)) {
    stop('"data" should be a data frame object')
  }

  if (any(c("tbl_dataf", "tbl") %in% class(data))) {
    warning('"data" is of type tibble\nCoercing to data frame')
    data <- as.data.frame(data)
  }

  # check if 'names' argument is character vector of unit length
  if (!(is.character(names) && length(names) == 1)) {
    stop('"names" should be a character vector of unit length')
  }

  # check if 'quantitative' argument is a character vector
  if (!is.null(quantitative)) {
    if (!is.character(quantitative)) {
      stop('"quantitative" should be a character vector')
    }
  }

  # check if 'qualitative' argument is a character vector
  if (!is.null(qualitative)) {
    if (!is.character(qualitative)) {
      stop('"qualitative" should be a character vector')
    }
  }

  # check if 'names' column is present in 'data'
  if (!(names %in% colnames(data))) {
    stop(paste('Column ', names,
               ' specified as the "names" column is not present in "data"',
               sep = ""))
  }

  # check if 'quantitative' columns are present in 'data'
  if (!is.null(quantitative)) {
    if (FALSE %in% (quantitative %in% colnames(data)))  {
      stop(paste('The following column(s) specified in "quantitative" not present in "data":\n',
                 paste(quantitative[!(quantitative %in% colnames(data))],
                       collapse = ", "),
                 sep = ""))
    }
  }

  # check if 'qualitative' columns are present in 'data'
  if (!is.null(qualitative)) {
    if (FALSE %in% (qualitative %in% colnames(data)))  {
      stop(paste('The following column(s) specified in "qualitative" not present in "data":\n',
                 paste(qualitative[!(qualitative %in% colnames(data))],
                       collapse = ", "),
                 sep = ""))
    }
  }

  # check if overlap exists between 'quantitative' and 'qualitative'
  if ((!is.null(quantitative)) & (!is.null(qualitative))) {
    if (length(intersect(quantitative, qualitative)) != 0) {
      stop(paste('The following column(s) is/are specified in "quantitative" and "qualitative":\n',
                 paste(intersect(quantitative, qualitative),
                       collapse = ", "),
                 sep = ""))
    }
  }

  # check if 'names' column is of type character
  if (!is.character(data[, names])) {
    stop('"names" column in "data" should be of type character')
  }


  # check if 'quantitative' columns are of type numeric/integer
  if (!is.null(quantitative)) {
    intquantcols <- unlist(lapply(data[, quantitative],
                                  function(x) FALSE %in% (is.vector(x, mode = "integer") | is.vector(x, mode = "numeric"))))
    if (TRUE %in% intquantcols) {
      stop(paste('The following "quantitative" column(s) in "data" are not of type numeric:\n',
                 paste(names(intquantcols[intquantcols]), collapse = ", ")))
    }
  }

  # check if 'qualitative' columns are of type factor
  if (!is.null(qualitative)) {
    intqualcols <- unlist(lapply(data[, qualitative],
                                 function(x) is.factor(x)))
    if (FALSE %in% intqualcols) {
      stop(paste('The following "qualitative" column(s) in "data" are not of type factor:\n',
                 paste(names(intqualcols[!intqualcols]), collapse = ", ")))
    }
  }

  # check for missing values
  missvcols <- unlist(lapply(data[, quantitative],
                             function(x) TRUE %in% is.na(x)))
  if (TRUE %in% missvcols) {
    stop(paste('The following column(s) in "data" have missing values:\n',
               paste(names(missvcols[missvcols]), collapse = ", ")))
  }

  # check for duplication in names
  if (any(duplicated(data[, names]))) {
    stop('Duplicated entries exist in "names" column')
  }

  # Prepare data ----

  dataf <- data[, c(names, quantitative, qualitative)]
  rownames(dataf) <- dataf[, names]
  dataf[, names] <- NULL

  pca_out <- NULL
  ca_out <- NULL
  famd_out <- NULL

  # PCA ----

  if (is.null(qualitative) & !is.null(quantitative)) {

    dataf <- dataf[, quantitative]

    dataf[, quantitative] <- lapply(dataf[, quantitative], function(x) {
      as.numeric(x)
    })

    ## Run PCA ----
    pca_out <- FactoMineR::PCA(X = dataf, scale.unit = TRUE,
                               ncp = length(quantitative),
                               graph = FALSE)

    ## Get Eigen values ----
    eig <- pca_out$eig[, "eigenvalue"]

    # round(sum(eig)) == length(quantitative)

    ## Get Loadings ----
    rot <- pca_out$svd$V

    ## Get Importance of factors/principal coordinates ----
    imp <-
      data.frame(# `Standard deviation` = pca_out$svd$vs,
        `Eigen value` = pca_out$eig[, "eigenvalue"],
        `Percentage of variance` = pca_out$eig[, "percentage of variance"],
        `Cumulative percentage of variance` = pca_out$eig[, "cumulative percentage of variance"],
        check.names = FALSE)

    rownames(imp) <- gsub("comp", "Dim", rownames(imp))

    ## Get Principal component scores ----
    scores <- pca_out$ind$coord

    ## Coordinates for biplot
    ind_coord <- pca_out$ind$coord
    ind_coord <- ind_coord[, 1:biplot.ndim]
    colnames(ind_coord) <- gsub("Dim.", "Dim ", colnames(ind_coord))

    var_coord <- pca_out$var$coord
    var_coord <- var_coord[, 1:biplot.ndim]
    colnames(var_coord) <- gsub("Dim.", "Dim ", colnames(var_coord))
    var_coord <- var_coord * biplot.trait.scale

  }

  # Run CA ----

  if (!is.null(qualitative) & is.null(quantitative)) {

    dataf <- dataf[, qualitative]

    dataf[, qualitative] <- lapply(dataf[, qualitative], function(x) {
      as.numeric(as.factor(x))
    })

    ## Run CA ----
    ca_out <- FactoMineR::CA(X = dataf,
                             ncp = length(qualitative),
                             graph = FALSE)

    ## Get Eigen values ----
    eig <- ca_out$eig[, "eigenvalue"]

    ## Get Loadings ----
    rot <- ca_out$svd$V

    ## Get Importance of factors/principal coordinates ----
    imp <-
      data.frame(# `Standard deviation` = ca_out$svd$vs[1:(length(qualitative) - 1)],
        `Eigen value` = ca_out$eig[, "eigenvalue"],
        `Percentage of variance` = ca_out$eig[, "percentage of variance"],
        `Cumulative percentage of variance` = ca_out$eig[, "cumulative percentage of variance"],
        check.names = FALSE)

    rownames(imp) <- gsub("dim", "Dim", rownames(imp))

    ## Get Principal component scores ----
    scores <- ca_out$row$coord

    ## Coordinates for biplot
    ind_coord <- ca_out$row$coord
    ind_coord <- ind_coord[, 1:biplot.ndim]

    var_coord <- ca_out$col$coord
    var_coord <- var_coord[, 1:biplot.ndim]
    var_coord <- var_coord * biplot.trait.scale

  }

  # Run FAMD ----

  if (!is.null(qualitative) & !is.null(quantitative)) {

    dataf[, quantitative] <- lapply(dataf[, quantitative], function(x) {
      as.numeric(x)
    })

    dataf[, qualitative] <- lapply(dataf[, qualitative], function(x) {
      as.factor(x)
    })

    ## Run FAMD ----
    famd_out <- FactoMineR::FAMD(base = dataf,
                                 ncp = length(c(quantitative, qualitative)),
                                 graph = FALSE)

    ## Get Eigen values ----
    eig <- famd_out$eig[, "eigenvalue"]

    # round(sum(eig)) == length(quantitative)

    ## Get Loadings ----
    rot <- famd_out$svd$V

    ## Get Importance of factors/principal coordinates ----
    imp <-
      data.frame(#`Standard deviation` = famd_out$svd$vs,
        `Eigen value` = famd_out$eig[, "eigenvalue"],
        `Percentage of variance` = famd_out$eig[, "percentage of variance"],
        `Cumulative percentage of variance` = famd_out$eig[, "cumulative percentage of variance"],
        check.names = FALSE)

    rownames(imp) <- gsub("comp", "Dim", rownames(imp))

    ## Get Principal component scores ----
    scores <- famd_out$ind$coord

    ## Coordinates for biplot
    ind_coord <- famd_out$ind$coord
    ind_coord <- ind_coord[, 1:biplot.ndim]
    colnames(ind_coord) <- gsub("Dim.", "Dim ", colnames(ind_coord))

    var_coord <- famd_out$var$coord
    var_coord <- var_coord[, 1:biplot.ndim]
    colnames(var_coord) <- gsub("Dim.", "Dim ", colnames(var_coord))
    var_coord <- var_coord * biplot.trait.scale

  }

  # Contribution of individuals/genotypes to total GSS ----

  if (is.null(eigen.threshold)) {
    eigen.threshold <- mean(eig)
  } else {
    if (!(any(eig >= eigen.threshold))) {
      eigen.threshold <- mean(eig)
      warning('There are no eigen values \u2265 "eigen.threshold".\n',
              'Using average of the eigen values (', round(eigen.threshold, 2),
              ') as "eigen.threshold"', sep = "")
    }
  }


  N <- nrow(scores)
  K <- max(which((eig > eigen.threshold)))

  Pi <- rowSums(scores[, 1:K] ^ 2)
  Pimax <- rowSums(scores ^ 2)

  CRi <- (Pi / (N * K))
  CRimax <- (Pimax / (N * K))

  CRi <- sort(CRi, decreasing = TRUE)
  CRimax <- sort(CRimax, decreasing = TRUE)

  cumCRi <- cumsum(CRi)
  cumCRimax <- cumsum(CRimax)

  # plot(cumCRimax, col = "green")
  # points(cumCRi, col = "red")

  eigen.threshold_label <- eigen.threshold
  if (nchar(eigen.threshold > 5)) {
    eigen.threshold_label <- round(eigen.threshold, 3)
  }

  # Plot eigen values ----
  eigdf <- data.frame(sl = seq_along(eig), eig = eig)
  eigdf$gp <- ifelse(eigdf$sl <= K, 1, 0)
  eigdf$gp <- as.factor(eigdf$gp)

  if (!is.null(screeplot.ndim)) {
    if (nrow(eigdf) > screeplot.ndim) {
      eigdf <- eigdf[1:screeplot.ndim, ]
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
    # geom_label(aes(x = sl, y = eig, label = round(eig, 3)),
    #            vjust = 1, hjust = -0.2) +
    # scale_x_continuous(breaks = eigdf$sl, limits = c(1, length(eig) + 0.5)) +
    xlab("Factors") +
    ylab("Eigen value") +
    theme_bw()

  # Select the core collection ----

  # Generalized sum of squares
  gssdf <- data.frame(Rank = seq_along(cumCRi),
                      VarRet = (cumCRi / max(cumCRi)) * 100)
  gssdf <- cbind(Id = rownames(gssdf), gssdf)
  rownames(gssdf) <- NULL

  ## By size specified ----
  size.sel <- ceiling(size * N)
  size.var <- gssdf[gssdf$Rank == size.sel, ]$VarRet

  size.segdf <- data.frame(x = c(-Inf, size.sel),
                           xend = c(size.sel, size.sel),
                           y = c(size.var, -Inf),
                           yend = c(size.var, size.var))
  size.segdf$label <- as.character(c(round(size.segdf[1, "y"], 2),
                                     size.segdf[2, "x"]))

  size.gssg <- ggplot(gssdf, aes(x = Rank, y = VarRet)) +
    geom_point() +
    geom_segment(data = size.segdf, aes(x = x, xend = xend,
                                        y = y, yend = yend),
                 colour = "red") +
    geom_text(data = size.segdf, aes(x = x, y = y, label = label),
              vjust = -0.5, hjust = -0.5, colour = "red") +
    scale_x_continuous(name = "Number of selected individuals" ,
                       sec.axis = sec_axis(transform = ~. / N,
                                           name = "Proportion of selected individuals")) +
    ylab("Variability retained (%)") +
    theme_bw()

  ## By threshold variance ----
  var.threshold <- var.threshold * 100
  var.sel <- max(which(gssdf$VarRet <= var.threshold))

  var.segdf <- data.frame(x = c(-Inf, var.sel),
                          xend = c(var.sel, var.sel),
                          y = c(var.threshold, -Inf),
                          yend = c(var.threshold, var.threshold))
  var.segdf$label <- as.character(c(round(var.segdf[1, "y"], 2),
                                    var.segdf[2, "x"]))

  var.gssg <- ggplot(gssdf, aes(x = Rank, y = VarRet)) +
    geom_point() +
    geom_segment(data = var.segdf, aes(x = x, xend = xend,
                                       y = y, yend = yend),
                 colour = "red") +
    geom_text(data = var.segdf, aes(x = x, y = y, label = label),
              vjust = -0.5, hjust = -0.5, colour = "red") +
    scale_x_continuous(name = "Number of selected individuals" ,
                       sec.axis = sec_axis(transform = ~. / N,
                                           name = "Proportion of selected individuals")) +
    ylab("Variability retained (%)") +
    theme_bw()

  ## With logistic regression ----

  # Fit a logistic model
  y <- gssdf$VarRet
  starta = y[1] / (1 - y[1])
  startb = -0.5

  maxiter = 1024
  warnOnly = TRUE

  dat <- data.frame(n = gssdf$Rank, y = gssdf$VarRet)


  mod <-
    gslnls::gsl_nls(
      y ~ 100 / (1 + exp(a + (b * n))),
      data = dat,
      algorithm = "lm",
      start = list(a = starta, b = startb),
      control = list(maxiter = maxiter, warnOnly = warnOnly,
                     scale = "levenberg")
    )

  # dat$pred <- 100 / (1 + exp(coef(mod)["a"] + (coef(mod)["b"] * dat$n)))

  # Compute rate of increase in variability retained
  dat$rate <- -coef(mod)["b"] * dat$y * (100 - dat$y)

  reg.sel <- dat[dat$rate == max(dat$rate), ]$n
  reg.var <- gssdf[gssdf$Rank == reg.sel, ]$VarRet

  reg.segdf <- data.frame(x = c(-Inf, reg.sel),
                          xend = c(reg.sel, reg.sel),
                          y = c(reg.var, -Inf),
                          yend = c(reg.var, reg.var))
  reg.segdf$label <- as.character(c(round(reg.segdf[1, "y"], 2),
                                    reg.segdf[2, "x"]))


  reg.gssg <- ggplot(gssdf, aes(x = Rank, y = VarRet)) +
    geom_point() +
    geom_segment(data = reg.segdf, aes(x = x, xend = xend,
                                       y = y, yend = yend),
                 colour = "red") +
    geom_text(data = reg.segdf, aes(x = x, y = y, label = label),
              vjust = -0.5, hjust = -0.5, colour = "red") +
    scale_x_continuous(name = "Number of selected individuals" ,
                       sec.axis = sec_axis(transform = ~. / N,
                                           name = "Proportion of selected individuals")) +
    ylab("Variability retained (%)") +
    theme_bw()

  reg.segdf2 <- data.frame(x = c(-Inf, reg.sel),
                           xend = c(reg.sel, reg.sel),
                           y = c(max(dat$rate), -Inf),
                           yend = c(max(dat$rate), max(dat$rate)))
  reg.segdf2$label <- as.character(c(round(reg.segdf2[1, "y"], 2),
                                     reg.segdf2[2, "x"]))

  reg.gssrateg <- ggplot(data = dat, aes(x = n, y = rate)) +
    geom_point() +
    geom_segment(data = reg.segdf2, aes(x = x, xend = xend,
                                        y = y, yend = yend),
                 colour = "red") +
    geom_text(data = reg.segdf2, aes(x = x, y = y, label = label),
              vjust = -0.5, hjust = -0.5, colour = "red") +
    scale_x_continuous(name = "Number of selected individuals" ,
                       sec.axis = sec_axis(transform = ~. / N,
                                           name = "Proportion of selected individuals")) +
    ylab("Rate of increase in variability retained (%)") +
    theme_bw()


  # Plot Biplot ----

  biplot_comb <- data.frame(t(combn(x = colnames(ind_coord), m = 2)))
  biplot_comb$label <- paste(biplot_comb$X1, biplot_comb$X2, sep = " vs. ")

  biplot_list <- vector("list", length = nrow(biplot_comb))
  names(biplot_list) <- biplot_comb$label

  for (i in seq_along(biplot_list)) {

    xlab <- paste(biplot_comb[i, 1], " (",
                  round(imp[biplot_comb[i, 1], ]$`Percentage of variance`, 2),
                  "% explained variance)", sep = "")
    ylab <- paste(biplot_comb[i, 2], " (",
                  round(imp[biplot_comb[i, 2], ]$`Percentage of variance`, 2),
                  "% explained variance)", sep = "")

    biplot_list[[i]] <- ggplot(data = ind_coord,
                               aes(x = .data[[biplot_comb[i, 1]]],
                                   y = .data[[biplot_comb[i, 2]]])) +
      geom_vline(xintercept = 0, linetype = 2, colour = "gray20") +
      geom_hline(yintercept = 0, linetype = 2, colour = "gray20") +
      geom_point(alpha = biplot.point.alpha) +
      geom_segment(data = var_coord, aes(x = 0, y = 0,
                                         xend = .data[[biplot_comb[i, 1]]],
                                         yend = .data[[biplot_comb[i, 2]]]),
                   arrow = arrow(length = unit(0.2, "cm")),
                   color = "red", alpha = biplot.segment.alpha) +
      geom_text_repel(data = var_coord, aes(x = .data[[biplot_comb[i, 1]]],
                                            y = .data[[biplot_comb[i, 2]]],
                                            label = rownames(var_coord)),
                      color = "red", vjust = -0.5) +
      xlab(label = xlab) +
      ylab(label = ylab) +
      theme_bw()
  }

  # patchwork::wrap_plots(biplot_list)

  # Generate ouput ----

  rawout_ind <- c(pca_out = !is.null(pca_out),
                  ca_out = !is.null(ca_out),
                  famd_out = !is.null(famd_out))

  detailsdf <-
    data.frame(`Quantitative traits` =  paste(quantitative, collapse = ", "),
              `Qualitative traits` =  paste(qualitative, collapse = ", "),
              `Threshold eigen value` = eigen.threshold,
              `Number of eigen values selected` = K,
              `Threshold size` = size,
              `Threshold variance (%)` = var.threshold * 100,
              check.names = FALSE)

  detailsdf <- t(detailsdf)
  detailsdf <- cbind(Detail = rownames(detailsdf),
                     Value = detailsdf[, 1])

  coreinfodf <- data.frame(Method = c("By size specified", "By threshold variance", "By logistic regression"),
                     Size = c(size.sel, var.sel, reg.sel),
                     VarRet = c(size.var, var.threshold, reg.var))

  out <- list(details = detailsdf,
              raw.out = get(names(which(rawout_ind))),
              eigen = eig,
              rotation = rot,
              scores = scores,
              variability.ret = gssdf,
              scree.plot = eigg,
              biplot = biplot_list,
              cores.info = coreinfodf,
              core.plots = list(by.size = size.gssg,
                                by.var = var.gssg,
                                by.reg = list(reg.gssg, reg.gssrateg)))

  class(out) <- "pcss.core"

  return(out)

}
