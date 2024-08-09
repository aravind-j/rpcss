
#' Principal Component Scoring to Generate Core collections
#'
#' Generate a Core Collection with Principal Component Scoring Strategy (PCSS)
#' \insertCite{noirot_principal_1996,noirot_method_2003}{rpcss} using
#' qualitative and/or quantitative trait data.
#'
#' For qualitative traits, dimensionality reduction is performed by Principal
#' Component Analysis (PCA). Similarly for quantitative traits, dimensionality
#' reduction is done via Multiple Correspondence Analysis (MCA). For cases where
#' both quantitative and qualitative traits are available, dimensionality
#' reduction is achieved by using the Factor Analysis for Mixed Data (FAMD)
#' method \insertCite{husson_Exploratory_2017}.
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
#' @param var.threshold The desired proportion of total variabilty to be
#'
#' @return A list of class \code{pcss.core} with the following components.
#'   \item{details}{The details of the core set generaton process.}
#'   \item{raw.out}{The original output of \code{\link[FactoMineR]{PCA}},
#'   \code{\link[FactoMineR]{CA}} and \code{\link[FactoMineR]{FAMD}} functions
#'   of \code{\link[FactoMineR]{FactoMineR}}} \item{eigen}{A data frame with
#'   eigen values and their partial and cumulative contribution to percentage of
#'   variance.} \item{eigen.threshold}{The threshold eigen value used.}
#'   \item{rotation}{A matrix of rotation values or loadings.} \item{scores}{A
#'   matrix of scores from PCA, CA or FAMD.} \item{variability.ret}{A data frame
#'   of genotypes ordered by variability retained.} \item{cores.info}{A data frame of core set
#'   size and percentage variablity retained according to the method used.}
#'   \item{core.plots}{A list of plots of cumulative variability retained by
#'   genotypes as \code{ggplot} objects. The core set size and corresponding
#'   percentage variability retained are highlighted according to the method
#'   used.}
#'
#' @seealso \code{\link[FactoMineR]{PCA}}, \code{\link[FactoMineR]{CA}} and
#'   \code{\link[FactoMineR]{FAMD}}
#'
#' @import ggplot2
#' @import gslnls
#' @importFrom FactoMineR PCA CA FAMD
#' @importFrom Rdpack reprompt
#' @export
#'
#' @examples
pcss.core <- function(data, names, quantitative, qualitative,
                      eigen.threshold = NULL,
                      size = 0.2, var.threshold = 0.75) {

  # Checks ----

  if (missing(quantitative)) {
    quantitative <- NULL
  }

  if (missing(qualitative)) {
    qualitative <- NULL
  }

  if (length(c(quantitative, qualitative)) == 1) {
    stop("Only one trait specified.")
  }

  # check if 'data' is a data frame object
  if (!is.data.frame(data)) {
    stop('"data" should be a data frame object.')
  }

  if (any(c("tbl_dataf", "tbl") %in% class(data))) {
    warning('"data" is of type tibble\nCoercing to data frame.')
    data <- as.data.frame(data)
  }

  # check if 'names' argument is character vector of unit length
  if (!(is.character(names) && length(names) == 1)) {
    stop('"names" should be a character vector of unit length.')
  }

  # check if 'quantitative' argument is a character vector
  if (!is.null(quantitative)) {
    if (!is.character(quantitative)) {
      stop('"quantitative" should be a character vector.')
    }
  }

  # check if 'qualitative' argument is a character vector
  if (!is.null(qualitative)) {
    if (!is.character(qualitative)) {
      stop('"qualitative" should be a character vector.')
    }
  }

  # check if 'names' column is present in 'data'
  if (!(names %in% colnames(data))) {
    stop(paste('Column ', names,
               ' specified as the "names" column is not present in "data".',
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
      stop(paste('The following column(s) is/are specified in both "quantitative" and "qualitative":\n',
                 paste(intersect(quantitative, qualitative),
                       collapse = ", "),
                 sep = ""))
    }
  }

  # check if 'names' column is of type character
  if (!is.character(data[, names])) {
    stop('"names" column in "data" should be of type character.')
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
    stop('Duplicated entries exist in "names" column.')
  }

  # check if 'eigen.threshold' argument is numeric vector of unit length
  if (!is.null(eigen.threshold)) {
    if (!(is.numeric(eigen.threshold) && length(eigen.threshold) == 1)) {
      stop('"eigen.threshold" should be a numeric vector of unit length.')
    }
  }

  # check if 'size' argument is numeric vector of unit length
  if (!is.null(size)) {
    if (!(is.numeric(size) && length(size) == 1)) {
      stop('"size" should be a numeric vector of unit length.')
    }
  }

  # check if 'size' is a proportion between 0 and 1
  if (size <= 0 | size >= 1) {
    stop('"size" should be a proportion between 0 and 1.')
  }

  # check if 'var.threshold' argument is numeric vector of unit length
  if (!is.null(var.threshold)) {
    if (!(is.numeric(var.threshold) && length(var.threshold) == 1)) {
      stop('"var.threshold" should be a numeric vector of unit length.')
    }
  }

  # check if 'var.threshold' is a proportion between 0 and 1
  if (var.threshold <= 0 | var.threshold >= 1) {
    stop('"var.threshold" should be a proportion between 0 and 1.')
  }

  # Prepare data ----

  dataf <- data[, c(names, quantitative, qualitative)]
  rownames(dataf) <- dataf[, names]
  dataf[, names] <- NULL

  pca_out <- NULL
  mca_out <- NULL
  famd_out <- NULL

  method <- NULL

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

    ## Method ----
    method <- "PCA"
  }

  # Run MCA ----

  if (!is.null(qualitative) & is.null(quantitative)) {

    dataf <- dataf[, qualitative]

    # dataf[, qualitative] <- lapply(dataf[, qualitative], function(x) {
    #   as.numeric(as.factor(x))
    # })

    # dataf[, qualitative] <- lapply(dataf[, qualitative], function(x) {
    #   as.factor(x)
    # })

    ncp <- sum(unlist(lapply(dataf, function(x) length(levels(x)))))

    ## Run MCA ----
    mca_out <- FactoMineR::MCA(X = dataf,
                               ncp = ncp,
                               graph = FALSE)

    ## Get Eigen values ----
    eig <- mca_out$eig[, "eigenvalue"]

    ## Get Loadings ----
    rot <- mca_out$svd$V

    ## Get Importance of factors/principal coordinates ----
    imp <-
      data.frame(# `Standard deviation` = mca_out$svd$vs[1:(length(qualitative) - 1)],
        `Eigen value` = mca_out$eig[, "eigenvalue"],
        `Percentage of variance` = mca_out$eig[, "percentage of variance"],
        `Cumulative percentage of variance` = mca_out$eig[, "cumulative percentage of variance"],
        check.names = FALSE)

    rownames(imp) <- gsub("dim", "Dim", rownames(imp))

    ## Get Principal component scores ----
    scores <- mca_out$ind$coord

    ## Method ----
    method <- "MCA"

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

    ## Method ----
    method <- "FAMD"
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


  # Generate ouput ----

  rawout_ind <- c(pca_out = !is.null(pca_out),
                  mca_out = !is.null(mca_out),
                  famd_out = !is.null(famd_out))

  detailsdf <-
    data.frame(`Quantitative traits` =  paste(quantitative, collapse = ", "),
               `Qualitative traits` =  paste(qualitative, collapse = ", "),
               `Method` = method,
               `Threshold eigen value` = eigen.threshold,
               `Number of eigen values selected` = K,
               `Threshold size` = size,
               `Threshold variance (%)` = var.threshold * 100,
               check.names = FALSE)

  detailsdf <- t(detailsdf)
  detailsdf <- cbind(Detail = rownames(detailsdf),
                     Value = detailsdf[, 1])

  coreinfodf <- data.frame(Method = c("By size specified",
                                      "By threshold variance",
                                      "By logistic regression"),
                           Size = c(size.sel, var.sel, reg.sel),
                           VarRet = c(size.var, var.threshold, reg.var))

  out <- list(details = detailsdf,
              raw.out = get(names(which(rawout_ind))),
              eigen = imp,
              eigen.threshold = eigen.threshold,
              rotation = rot,
              scores = scores,
              variability.ret = gssdf,
              cores.info = coreinfodf,
              core.plots = list(by.size = size.gssg,
                                by.var = var.gssg,
                                by.reg = list(reg.gssg, reg.gssrateg)))

  class(out) <- "pcss.core"

  attr(x = out, which = "method") <- method
  attr(x = out, which = "quant") <- quantitative
  attr(x = out, which = "quali") <- qualitative

  return(out)

}
