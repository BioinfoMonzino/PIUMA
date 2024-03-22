#' @title Assessment of Scale-Free model fitting
#'
#' @description This function assesses the fitting to a scale-free net model.
#'
#' @param x A TDAobj object, processed by the  \code{\link{jaccardMatrix}}
#'
#' @param showPlot A string indicating whether the plot has to be
#' generated or not. Only 'yes' (default) or 'no' are allowed
#'
#' @return A list containing:
#' \itemize{
#'    \item the estimated gamma value
#'    \item The correlation between the k and the degree distribution p(k).
#'    \item The p-value of the correlation between the k and the degree
#'    distribution p(k).
#'    \item The correlation between the logarithm (base 10) of k and the
#'    logarithm (base 10) of the degree distribution p(k).
#'    \item The p-value of the correlation between the logarithm (base 10) of k
#'    and the logarithm (base 10) of the degree distribution p(k).
#'    }
#'
#' @details The scale-free networks show a high negative correlation beween k
#' and p(k).
#'
#' @author  Mattia Chiesa, Laura Ballarini,Luca Piacentini
#'
#' @examples
#' ## use example data:
#' data(tda_test_data)
#' netModel <- checkScaleFreeModel(tda_test_data, "no")
#'
#' @seealso
#' \code{\link{makeTDAobj}},
#' \code{\link{dfToDistance}},
#' \code{\link{dfToProjection}},
#' \code{\link{mapperCore}},
#' \code{\link{jaccardMatrix}}
#'
#' @export
#'
#'
checkScaleFreeModel <- function(x, showPlot=c("yes","no")) {

  # checks----------------------------------------------------------------------
  if (!is(x,'TDAobj'))
    stop("'x' argument must be a TDAobj object")

  jaccIndexes <- x@jacc

  # check missing arguments
  if (missing(jaccIndexes))
    stop("'jaccIndexes' argument must be provided")

  if (missing(showPlot)) {
    showPlot <- showPlot[2]
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

  if (!(showPlot %in% c("yes", "no")))
    stop("'showPlot' must be yes or no")

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

    powLawFit <- fit_power_law(dataPl+1, round(max(dataPl)/5))
    infoModelRes[['gamma']] <- powLawFit$alpha

    logk <- log10(dataPlDist$k)
    logpk <- log10(dataPlDist$pk)

    k <- (dataPlDist$k)
    pk <- (dataPlDist$pk)


    if (length(dataPlDist$k) >= 5) {
      # correlation between k and pk
      rCorkpk <- rcorr(k, pk)
      rCorlogklogpk <- rcorr(logk, logpk)

      infoModelRes[['corkpk']] <- rCorkpk[["r"]][2]
      infoModelRes[['pValkpk']] <-rCorkpk[["P"]][2]
      infoModelRes[['corlogklogpk']] <- rCorlogklogpk[["r"]][2]
      infoModelRes[['pVallogklogpk']] <-rCorlogklogpk[["P"]][2]
    }else{
      infoModelRes[['corkpk']] <- NA
      infoModelRes[['pValkpk']] <- NA
      infoModelRes[['corlogklogpk']] <- NA
      infoModelRes[['pVallogklogpk']] <-NA
      warning("length(dataPlDist$k) < 5, hence rcorr cannot be computed.
              'corkCk', 'pValkCk' in the returned list are set to NA")
    }


    # plot----------------------------------------------------------------------
    if (showPlot %in% c("yes")) {

      # degree distribution plot
      p1 <- ggplot(dataPlDist) +
        geom_point(aes(x=k, y=pk), color='blue', size=3) +
        stat_smooth(data=dataPlDist,
                    aes(x = k, y = pk),
                    fill="orange1",
                    method = "lm",
                    color="orange",
                    size=1,
                    alpha=0.1)+

        geom_line(aes(x=k, y=pk), color='purple', linetype = "dashed") +
        theme_bw()+
        ggtitle(paste0("Degree distr. (linear scale) --- R = ",
                       round(rCorkpk[["r"]][2],2))
        )+
        theme(plot.title = element_text(size = 10, face = "bold")) +
        scale_x_continuous(breaks= pretty_breaks())

      p2 <- p1 + scale_x_log10(
        breaks = trans_breaks("log10", function(x) 10^x),
        labels = trans_format("log10", math_format(10^.x))
      ) +
        scale_y_log10(
          breaks = trans_breaks("log10", function(x) 10^x),
          labels = trans_format("log10", math_format(10^.x))
        ) +
        annotation_logticks()+
        ggtitle(paste0("Degree distr. (log-log scale) --- R = ",
                       round(rCorlogklogpk[["r"]][2],2))
        )+
        theme(plot.title = element_text(size = 10, face = "bold"))


      show(wrap_plots(p1, p2))
    }

  } else { # else related to if (max(dataPl) >= 5)

    infoModelRes[['gamma']] <- NA
    infoModelRes[['corkpk']] <- NA
    infoModelRes[['pValkpk']] <- NA
    infoModelRes[['corlogklogpk']] <- NA
    infoModelRes[['pVallogklogpk']] <-NA

    warning("max(degree of the net) < 5, hence the returned list has all
            NA values and plot is not shown")
  }

  return(infoModelRes)
}



#' @title Compute the Network Entropy
#'
#' @description This function computes the average of the entropies for
#' each node of a network.
#'
#' @param outcome_vect A vector containing the average outcome values for each
#' node
#' of a network.
#'
#' @return The network entropy using each node of a network.
#'
#' @details The average of the entropies is related to the amount of information
#' stored in the network.
#'
#' @author Mattia Chiesa, Laura Ballarini, Luca Piacentini
#'
#' @examples
#' # use example data:
#' set.seed(1)
#' entropy <- checkNetEntropy(round(runif(10),0))
#'
#' @seealso
#' \code{\link{makeTDAobj}},
#' \code{\link{dfToDistance}},
#' \code{\link{dfToProjection}},
#' \code{\link{mapperCore}},
#' \code{\link{jaccardMatrix}},
#' \code{\link{tdaDfEnrichment}}
#'
#' @export
#'
checkNetEntropy <- function(outcome_vect) {

  # checks----------------------------------------------------------------------
  # check missing arguments
  if (missing(outcome_vect))
    stop("'outcome_vect' argument must be provided")

  # check the type of argument
  if (!is.numeric(outcome_vect))
    stop("'outcome_vect' argument must be numeric")

  # check the presence of NA or Inf
  if (any(is.na(outcome_vect)))
    stop("NA values are not allowed in the 'vectFreq'")

  if (any(is.infinite((outcome_vect))))
    stop("Inf values are not allowed in the 'vectFreq'")

  # body -----------------------------------------
  outcome_vect_r <- round(outcome_vect,0)
  uniq_classes <- unique(sort(outcome_vect_r))

  p_logp <- c()
  for (i in uniq_classes){
    prob_class <- length(which(outcome_vect_r %in% i ))/length(outcome_vect_r)
    p_logp[i] <- prob_class * log(prob_class, base = 2)

  }
  outcome_entropy <- round(-sum(p_logp), 3)
  return(outcome_entropy)

}
