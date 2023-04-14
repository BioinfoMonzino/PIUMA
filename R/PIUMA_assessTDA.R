#' @title Assessment of Scale-Free model fitting
#'
#' @description This function assesses the fitting to a scale-free net model.
#'
#' @param jaccIndexes A matrix of the Jaccard indexes between pair of nodes.
#' @param showPlot A string indicating the type of plot to be shown.
#' Only: "log", "linear" and "null" values are allowed. Each graph represent
#' the degree distribution p(k) and the average clustering coefficient of all
#' nodes (jaccIndexes rows/cols) with k links C(k).
#' "log": graphs are shown in logarithmic scale;
#' "linear": graphs are shown in the linear scale;
#' "null": graphs are not shown. Default: "log".
#'
#' @return A list containing:
#' \itemize{
#'    \item The average path length.
#'    \item The number of network nodes.
#'    \item The correlation between the logarithm (base 10) of k and the
#'    logarithm (base 10) of the degree distribution p(k).
#'    \item The p-value of the correlation between the logarithm (base 10) of k
#'    and the logarithm (base 10) of the degree distribution p(k).
#'    \item The global transitivity (i.e. global clustering coefficient).
#'    \item The correlation between the logarithm (base 10) of k
#'    and the logarithm (base 10) of the average clustering coefficient
#'    of all nodes with k links C(k).
#'    \item The p-value of the correlation between the logarithm (base 10) of k
#'    and the logarithm (base 10) of the average clustering coefficient
#'    of all nodes with k links C(k).
#'    }
#'
#' @details The scale-free networks show a high negative correlation beween k 
# ' and p(k).
#'
#' @author Laura Ballarini, Mattia Chiesa
#'
#' @examples
#' ## use example data:
#' data(jacc_mat_tda)
#' netModel <- checkScaleFreeModel(jacc_mat_tda, "null")
#'
#' @seealso
#' \code{\link{importSplitScale}},
#' \code{\link{dfToDistance}},
#' \code{\link{dfToProjection}},
#' \code{\link{mapperCore}},
#' \code{\link{jaccardMatrix}}
#'
#' @export
#'
#'
checkScaleFreeModel <- function(jaccIndexes,
                                showPlot=c("log", "linear","null")) {

  # checks----------------------------------------------------------------------

  # check missing arguments
  if (missing(jaccIndexes))
    stop("'jaccIndexes' argument must be provided")

  if (missing(showPlot)) {
    showPlot <- showPlot[1]
  }

  # check the type of argument
  if (!is.matrix(jaccIndexes))
    stop("'jaccIndexes' argument must be a matrix")

  if (!is.character(showPlot))
    stop("'showPlot' argument must be character")

  # specific checks
  if (!all((jaccIndexes >= 0 & jaccIndexes <= 1) | is.na(jaccIndexes)))
    stop("'jaccIndexes' must be in [0; 1] or NA")

  if (length(showPlot) > 1)
    stop("length(showPlot) must be equal to 1")

  if (!(showPlot %in% c("log", "linear", "null")))
    stop("'showPlot' must be one of 'log', 'linear', 'null'")

  # check the presence of Inf values
  if (any(is.infinite(jaccIndexes)))
    stop("Inf values are not allowed in the 'jaccIndexes' matrix")

  # Power Law Degree distribution (body)----------------------------------------

  infoModelRes <- list()

  # create the graph
  adjDataRes <- jaccIndexes
  adjDataRes[adjDataRes > 0] <- 1
  graphFromAdjMatr <- graph_from_adjacency_matrix(adjDataRes,
                                                  mode = "undirected")

  # compute the degree and pk
  dataPl <- degree(graphFromAdjMatr, mode="in")
  dataPlDist <- data.frame(k=0:max(dataPl),
                           pk=degree_distribution(graphFromAdjMatr))
  dataPlDist <- dataPlDist[dataPlDist$pk > 0 & dataPlDist$k > 0,]

  if (max(dataPl) >= 5) {

    # average path length
    numNodes <- gorder(graphFromAdjMatr)
    averPathLen <- average.path.length(graphFromAdjMatr)
    infoModelRes[['averPathLen']] <- averPathLen
    infoModelRes[['nNodes']] <- numNodes

    # # get alpha (alpha=gamma, p(k)~k^-gamma)
    # powLawFit <- fit_power_law(dataPl+1, round(max(dataPl)/5))
    # infoModelRes[['gamma']] <- powLawFit$alpha

    logk <- log10(dataPlDist$k)
    logpk <- log10(dataPlDist$pk)
    if (length(dataPlDist$k) >= 5) {
      # correlation between k and pk
      rCorkpk <- rcorr(logk, logpk)
      infoModelRes[['corkpk']] <- rCorkpk[["r"]][2]
      infoModelRes[['pValkpk']] <-rCorkpk[["P"]][2]
    }else{
      infoModelRes[['corkpk']] <- NA
      infoModelRes[['pValkpk']] <- NA
      warning("length(dataPlDist$k) < 5, hence rcorr cannot be computed.
              'corkCk', 'pValkCk' in the returned list are set to NA")
    }

    # clustering coefficient
    clustCoeffGlobal <- transitivity(graphFromAdjMatr, "global")
    infoModelRes[['globalTransit']] <-  clustCoeffGlobal

    clustCoeffLocal <- transitivity(graphFromAdjMatr, "local")
    transitGraph <- data.frame(k=min(dataPl):max(dataPl),
                               sumCk=rep(0, length(min(dataPl):max(dataPl))),
                               tot=rep(0, length(min(dataPl):max(dataPl))),
                               Ck=rep(0, length(min(dataPl):max(dataPl))))
    # compute the mean local transitivity (Ck column in 'transitGraph')
    for (i in seq_len(length(clustCoeffLocal))){
      transitGraph[dataPl[i]+1,2] <- transitGraph[dataPl[i]+1,2]+
        clustCoeffLocal[i]
      transitGraph[dataPl[i]+1,3] <- transitGraph[dataPl[i]+1,3]+1
    }
    for (i in c(0,seq_len(dim(transitGraph)[1]))){
      transitGraph[i,4] <- transitGraph[i,2]/transitGraph[i,3]
    }
    transitGraph <- transitGraph[transitGraph$k > 1 & transitGraph$Ck > 0 &
                                   !is.na(transitGraph$Ck) &
                                   !is.infinite(transitGraph$Ck), ]

    logTGk <- log10(transitGraph$k)
    logCk <- log10(transitGraph$Ck)
    if (length(transitGraph$k) >= 5) {
      # correlation between k and Ck
      rCorkCk <- rcorr(logTGk, logCk)
      infoModelRes[['corkCk']] <- rCorkCk[["r"]][2]
      infoModelRes[['pValkCk']] <- rCorkCk[["P"]][2]
    }else{
      infoModelRes[['corkCk']] <- NA
      infoModelRes[['pValkCk']] <- NA
      warning("length(transitGraph$k) < 5, hence rcorr cannot be computed.
              'corkCk', 'pValkCk' in the returned list are set to NA")
    }

    # plot----------------------------------------------------------------------
    if (showPlot %in% c("log", "linear")) {

      # degree distribution plot
      p1 <- ggplot(dataPlDist) +
        geom_point(aes(x=k, y=pk), color='blue', size=3) +
        geom_line(linewidth=0.7, aes(x=k, y=pk), color='cyan', linetype =
                    "dashed") +
        theme_bw()+
        ggtitle("Degree distribution")
      if(showPlot %in% "log"){
        p1 <- p1 + scale_x_log10(
          # breaks = trans_breaks("log10",
          #                                              function(x) 10^x),
          #                        labels = trans_format("log10",
          #                                              math_format(10^.x)))
                                 ) +
          scale_y_log10(
            # breaks = trans_breaks("log10", function(x) 10^x),
            #             labels = trans_format("log10", math_format(10^.x)))
                        ) +
          annotation_logticks()
      }

      # local transitivity plot
      p2 <- ggplot(transitGraph) +
        geom_point(aes(x=k, y=Ck), color='blue', size=3) +
        geom_line(linewidth=0.7, aes(x=k, y=Ck), color="cyan",
                  linetype = "dashed")+
        geom_line(linewidth=0.65, aes(x=k, y=k^-1.5, color="k^-1.5"),
                  linetype = "solid")+
        geom_line(linewidth=0.65, aes(x=k, y=k^-1, color="k^-1"),
                  linetype = "solid")+
        geom_line(linewidth=0.65, aes(x=k, y=k^-0.5, color="k^-0.5"),
                  linetype = "solid")+
        scale_color_manual(name = "C(k)~k^-1", values = c("k^-1.5" = "green",
                                                           "k^-1" = "black",
                                                           "k^-0.5" = "red"))+
        theme_bw()+
        ggtitle("Local transitivity")
      if(showPlot %in% "log"){
        p2 <- p2 + scale_x_log10(
          # breaks = trans_breaks("log10",
          #                                              function(x) 10^x),
          #                        labels = trans_format("log10",
          #                                              math_format(10^.x)))
                                 ) +
          scale_y_log10(
            # breaks = trans_breaks("log10", function(x) 10^x),
            #             labels = trans_format("log10", math_format(10^.x)))
                        ) +
          annotation_logticks()
      }

      print(p1 + p2)
    }

  } else { # else related to if (max(dataPl) >= 5)
    infoModelRes[['averPathLen']] <- NA
    infoModelRes[['nNodes']] <- NA
    #infoModelRes[['gamma']] <- NA
    infoModelRes[['corkpk']] <- NA
    infoModelRes[['pValkpk']] <- NA
    infoModelRes[['globalTransit']] <-  NA
    infoModelRes[['corkCk']] <- NA
    infoModelRes[['pValkCk']] <- NA
    warning("max(degree of the net) < 5, hence the returned list has all
            NA values and plot is not shown")
  }

  return(infoModelRes)
}


#' @title Compute the Network Entropy (only for binary outcome!)
#'
#' @description This function computes the average of the entropies computed for
#' each node of a network, only for variables whose elements range (0;1)
#'
#' @param vectFreq A vector containing the frequency of the outcome
#' (i.e. p(outcome=1) ) for each node of a network.
#'
#' @return The average of the entropies computed for each node of a network.
#'
#' @details The average of the entropies is related to the amount of information
#' stored in the network.
#'
#' @author Laura Ballarini, Mattia Chiesa
#'
#' @examples
#' # use example data:
#' set.seed(1)
#' entropy <- checkNetEntropy(runif(5))
#'
#' @seealso
#' \code{\link{importSplitScale}},
#' \code{\link{dfToDistance}},
#' \code{\link{dfToProjection}},
#' \code{\link{mapperCore}},
#' \code{\link{jaccardMatrix}},
#' \code{\link{tdaDfEnrichment}}
#'
#' @export
#'
checkNetEntropy <- function(vectFreq) {

  # checks----------------------------------------------------------------------
  # check missing arguments
  if (missing(vectFreq))
    stop("'vectFreq' argument must be provided")

  # check the type of argument
  if (!is.numeric(vectFreq))
    stop("'vectFreq' argument must be numeric")

  # specific checks
  if (!all((vectFreq >= 0 & vectFreq <= 1)))
    stop("'vectFreq' elements must be in [0; 1]")

  # check the presence of NA or Inf
  if (any(is.na(vectFreq)))
    stop("NA values are not allowed in the 'vectFreq'")

  if (any(is.infinite((vectFreq))))
    stop("Inf values are not allowed in the 'vectFreq'")

  # 'meanNetEntropy' computation (body)-----------------------------------------
  entropyVect <- c()
  for (i in seq_len(length(vectFreq))) {
    if(vectFreq[i] == 0 | vectFreq[i] == 1) {
      entropyVect[i] <- 0
    }else{
      # -[p(0)*log2(p(0)) + p(1)*log2(p(1))]
      entropyVect[i] <- -((1-vectFreq[i])*log2(1-vectFreq[i]) +
                             (vectFreq[i])*log2(vectFreq[i]))
    }

  }
  meanNetEntropy <- mean(entropyVect)
  return(meanNetEntropy)
}
