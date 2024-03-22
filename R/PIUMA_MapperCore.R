#' @title Implement the TDA Mapper algorithm on TDAobj
#'
#' @description This is a comprehensive function permitting to perform the core
#' TDA Mapper algorithm with 2D lenses. It allow setting several types of
#' clustering methods.
#'
#' @param x A TDAobj object, processed by the  \code{\link{dfToDistance}} and
#' \code{\link{dfToProjection}} functions.
#' @param nBins The number of bins (i.e. the resolution of the cover).
#' Default: 15.
#' @param overlap The overlap between bins (i.e.the gain of the cover).
#' Default: 0.4.
#' @param mClustNode The number of clusters in each overlapping bin. Default: 2
#' @param remEmptyNode A logical value to remove or not the empty nodes from
#' the resulting data.frame. Default: TRUE.
#' @param clustMeth The clustering algorithm."HR", "kmeans", "DBSCAN", and
#'  "OPTICS" are allowed. Default: "kmeans".
#' @param HRMethod The name of the linkage criterion (when clustMeth="HR").
#' "average" and "complete" values are allowed. Default: "average".
#'
#' @return The starting TDAobj object, in which the result of mapper algorithm
#' (inferred nodes with their elements) has been added (slot: 'dfMapper')
#'
#' A data.frame containing the clusters, with their elements,
#'  identified by TDA .
#'
#'
#' @author Mattia Chiesa, Laura Ballarini, Luca Piacentini
#'
#' @examples
#' # use example data:
#' data(tda_test_data)
#' set.seed(1)
#' dfMapper <- mapperCore(tda_test_data, nBins=5, overlap=0.5,
#' mClustNode=2, clustMeth="kmeans")
#'
#' @seealso
#' \code{\link{makeTDAobj}},
#' \code{\link{dfToDistance}},
#' \code{\link{dfToProjection}}
#'
#' @export
#'
mapperCore <- function (x, nBins = 15, overlap = 0.4,
                        mClustNode = 2, remEmptyNode = TRUE,
                        clustMeth = c("kmeans","HR", "DBSCAN", "OPTICS"),
                        HRMethod = c("average", "complete")) {

  # checks----------------------------------------------------------------------
  if (!is(x,'TDAobj'))
    stop("'x' argument must be a TDAobj object")

  dfDistances <- x@dist_mat
  df2Dlens <- x@comp
  # check missing arguments
  if (missing(dfDistances))
    stop("'dfDistances' argument must be provided")

  if (missing(df2Dlens))
    stop("'df2Dlens' argument must be provided")

  if (missing(clustMeth)) {
    clustMeth <- clustMeth[1]
  }

  if (missing(HRMethod)) {
    HRMethod <- HRMethod[1]
  }

  # check the type of argument
  if (!is.data.frame(dfDistances))
    stop("'dfDistances' argument must be a data.frame")

  if (!is.data.frame(df2Dlens))
    stop("'df2Dlens' argument must be a data.frame")

  if (!is.numeric(nBins))
    stop("'nBins' argument must be numeric")

  if (!is.numeric(overlap))
    stop("'overlap' argument must be numeric")

  if (!is.numeric(mClustNode))
    stop("'mClustNode' argument must be numeric")

  if (!is.logical(remEmptyNode))
    stop("'remEmptyNode' argument must be logical")

  if (!is.character(clustMeth))
    stop("'clustMeth' argument must be a string")

  if (!is.character(HRMethod))
    stop("'HRMethod' argument must be a string")

  # specific checks
  if (dim(dfDistances)[1] != dim(df2Dlens)[1])
    stop("n. of rows 'dfDistances' must be equal to n. of rows 'df2Dlens'")

  if(dim(df2Dlens)[2] != 2)
    stop("dim(df2Dlens)[2] must be equal to 2")

  if (dim(dfDistances)[1] < 10 & dim(df2Dlens)[1] < 10)
    stop("n. 'dfDistances' and 'df2Dlens' rows greater than 10")

  if (!all(vapply(dfDistances, class,
                  FUN.VALUE = character(1)) %in% c("numeric","integer")))
    stop("'dfDistances' variables must be numeric")

  if (!all(vapply(df2Dlens, class, FUN.VALUE = character(1)) %in% c("numeric",
                                                                    "integer")))
    stop("'df2Dlens' variables must be numeric")

  if (!isSymmetric.matrix(as.matrix(dfDistances)))
    stop("'dfDistances' is not symmetric. Please be sure it is a dataframe of
            distances")

  if (isSymmetric.matrix(as.matrix(dfDistances)) &
      sum(diag(as.matrix(dfDistances))) != 0)
    stop("'dfDistances' is symmetric, but with trace != 0. Please be sure it
            is a dataframe of distances (dissimilarities)")

  if (length(nBins) > 1)
    stop("length(nBins) must be equal to 1")

  if ((nBins %% 1) != 0)
    stop("nBins must be integers")

  if (!(nBins>1 & nBins <= 100))
    stop("nBins must be in [2; 100]")

  if (length(overlap) > 1)
    stop("length(overlap) must be equal to 1")

  if (!(overlap > 0 & overlap < 1))
    stop("'overlap' must be in (0; 1)")

  if (length(mClustNode) > 1)
    stop("length(mClustNode) must be equal to 1")

  if ((mClustNode %% 1) != 0)
    stop("'mClustNode' must be an integer")

  if (!(mClustNode >=1 & mClustNode <= 15))
    stop("'mClustNode' must be [1; 15]")

  if (length(remEmptyNode) > 1)
    stop("length(remEmptyNode) must be equal to 1")

  if (length(clustMeth) > 1)
    stop("length(clustMeth) must be equal to 1")

  if (!(clustMeth %in% c("kmeans","HR", "DBSCAN", "OPTICS")))
    stop("'clustMeth' must be one of 'kmeans', 'HR', 'DBSCAN', 'OPTICS'")

  if (length(HRMethod) > 1)
    stop("length(HRMethod) must be equal to 1")

  if (!(HRMethod %in% c("average", "complete")))
    stop("'HRMethod' must be one of 'average', 'complete'")

  # check the presence of NA or Inf
  if (any(is.na(dfDistances)))
    stop("NA values are not allowed in the 'dfDistances' data.frame")

  if (any(is.infinite(as.matrix(dfDistances))))
    stop("Inf values are not allowed in the 'dfDistances' data.frame")

  if (any(is.na(df2Dlens)))
    stop("NA values are not allowed in the 'df2Dlens' data.frame")

  if (any(is.infinite(as.matrix(df2Dlens))))
    stop("Inf values are not allowed in the 'df2Dlens' data.frame")

  # initializations-------------------------------------------------------------
  nBins[2] <- nBins
  pointsInNodeDf <- as.data.frame(matrix(nrow = 0, ncol = 1))
  # counter for pointsInNodeDf
  k <- 1

  # define the sectors (used in for main loop)----------------------------------

  minFilt1 <- min(df2Dlens[, 1])
  maxFilt1 <- max(df2Dlens[, 1])
  minFilt2 <- min(df2Dlens[, 2])
  maxFilt2 <- max(df2Dlens[, 2])

  # interval length  for axis x (lenBin1) and y (lenBin2)
  lenBin1 <- (maxFilt1 - minFilt1)/(nBins[1] - (nBins[1] - 1) * overlap)
  lenBin2 <- (maxFilt2 - minFilt2)/(nBins[2] - (nBins[2] - 1) * overlap)

  stepSize1 <- lenBin1 * (1 - overlap)
  stepSize2 <- lenBin2 * (1 - overlap)

  numSectors <- nBins[1] * nBins[2]

  sectorIdx1 <- rep(seq_len(nBins[1]), nBins[2])
  sectorIdx2 <- rep(seq_len(nBins[2]), each = nBins[1])

  # main loop-------------------------------------------------------------------
  for (sector in seq_len(numSectors)) {

    sector1 <- sectorIdx1[sector]
    sector2 <- sectorIdx2[sector]

    minValueInSector1 <- minFilt1 + (sector1 - 1) * stepSize1
    minValueInSector2 <- minFilt2 + (sector2 - 1) * stepSize2

    maxValueInSector1 <- minValueInSector1 + lenBin1
    maxValueInSector2 <- minValueInSector2 + lenBin2

    # find points in sector
    pointsInSectorLogical <- (minValueInSector1 <= df2Dlens[,1]) &
      (minValueInSector2 <= df2Dlens[,2]) &
      (df2Dlens[,1] <= maxValueInSector1) &
      (df2Dlens[,2] <= maxValueInSector2)
    pointsInSectorIdx <- rownames(df2Dlens[which(pointsInSectorLogical),])

    numPointsInSector <- sum(pointsInSectorLogical)

    # if to build the resulting df (i.e. 'pointsInNodeDf')
    if (numPointsInSector > mClustNode) {
      sectorDfDistances <- as.dist(as.matrix(dfDistances)
                                   [pointsInSectorLogical,
                                     pointsInSectorLogical])

      # clustering
      switch(clustMeth, "HR"={

        HRClust <- hclust(sectorDfDistances, method=HRMethod)
        # cut the dendrogram in order to create the desired number of clusters
        dfClust <- cutree(HRClust, k = mClustNode)

      }, "kmeans"={

        clustInSector <- kmeans(sectorDfDistances, mClustNode, iter.max = 10,
                                nstart = 1, trace = FALSE)
        dfClust<-clustInSector$cluster

      }, "DBSCAN"={

        # find knn
        knnValY <- sort(kNNdist(sectorDfDistances,
                                round(log(dim(as.matrix(sectorDfDistances))[1])
                                      )
                                )
                        )
        knnPointsX <- seq(from = 1, to = length(knnValY))

        # interpolation function
        apprFunct <- approx(knnPointsX, knnValY, n = 1000)
        knnPointsX <- apprFunct$x
        knnValY <- apprFunct$y

        # second derivative computation
        der1 <- diff(knnValY) / diff(knnPointsX)
        der2 <- diff(der1) / diff(knnPointsX[-1])

        # find the elbow point
        indices <- which(der2 == max(abs(der2)))
        elbow <- knnValY[indices]
        if(length(elbow) > 1) {
          elbow <- mean(elbow)
        }else if(length(elbow) < 1) {
          elbow <- mean(knnValY)
        }

        clustInSector <- dbscan(sectorDfDistances,
                                eps = elbow,
                      minPts = round(log(dim(as.matrix(sectorDfDistances))[1])))
        dfClust <- clustInSector$cluster

      }, "OPTICS"={

        optClust <- optics(sectorDfDistances,
                    minPts = round(log(dim(as.matrix(sectorDfDistances))[1])))
        clustInSector <- extractDBSCAN(optClust, eps_cl = (optClust$eps/2))
        dfClust <- clustInSector$cluster

      })

      dfSectorCluster <- cbind(pointsInSectorIdx, dfClust)
      clusterUniq <- sort(unique(dfSectorCluster[, 2]))

      for (i in seq_len(length(clusterUniq))) {
        pointsInNodeDf[k, 1] <- gsub(",", "", toString(dfSectorCluster[
          which(dfSectorCluster[, 2] == i), 1]))
        rownames(pointsInNodeDf)[k] <- paste0("node_", sector, "_cl_", i)
        k <- k + 1
      }

    } else {

      pointsInNodeDf[k, 1] <- gsub(",","", toString(pointsInSectorIdx))
      rownames(pointsInNodeDf)[k] <- paste0("node_", sector, "_cl_1")
      k <- k + 1

    } # end if(numPointsInSector > mClustNode)/else
  }  # end main loop
  colnames(pointsInNodeDf) <- "sample_x_cluster"

  # remove empty nodes----------------------------------------------------------
  if (remEmptyNode==TRUE) {
    '%!in%' <- function(x,y)!('%in%'(x,y))
    pointsInNodeDf <- pointsInNodeDf[which(pointsInNodeDf[,1] %!in% ""),,
                                     drop=FALSE]
  }

  # remove identical nodes (only shared samples): leave-one-out
  uniq_nodes <- unique(pointsInNodeDf$sample_x_cluster)
  for (i in seq_len(length(uniq_nodes))){
    idx <- which(pointsInNodeDf$sample_x_cluster %in% uniq_nodes[i])
    if(length(idx) > 1){
      idx_bad <- idx[2:length(idx)]
      pointsInNodeDf[idx_bad, 1] <- NA
    }
  }
  pointsInNodeDf <- pointsInNodeDf[complete.cases(pointsInNodeDf),,drop=FALSE]

  x@dfMapper <- pointsInNodeDf

  return(x)
}
