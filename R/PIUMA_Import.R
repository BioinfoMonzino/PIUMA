#' @title Import data and generate the TDAobj object
#'
#' @description This function import a data.frame and create the object to
#' store all data needed for TDA analysis. In addition, some preliminary
#' preprocess steps are performed; specifically, outcomes variables data will
#' be separated the rest of dataset. The remaining dataset will be also
#' re-scaled (0-1)
#'
#' @param df A data.frame representing a dataset in the classical n x m form.
#' Rows (n) and columns (m) should be, respectively, observations and features.
#' @param outcomes A string or vector of string containing the name of variables
#' that have to be considered 'outcomes'
#'
#' @return A TDA object containing:
#' \itemize{
#'   \item  orig_data A data.frame of original data (without outcomes)
#'   \item  scaled_data A data.frame of re-scaled data (without outcomes)
#'   \item  outcomeFact A data.frame of original outcomes
#'   \item  outcome A data.frame of original outcomes converted as numeric
#'   \item  comp A data.frame containing the components of projected data
#'   \item  dist_mat A data.frame containing the computed distance matrix
#'   \item  dfMapper A data.frame containing the nodes, with their elements,
#' identified by TDA
#'   \item  jacc A matrix of Jaccard indexes between each pair of dfMapper nodes
#'   \item  node_data_mat A data.frame with the node size and the average value
#'   }
#'
#' @author  Mattia Chiesa, Laura Ballarini, Luca Piacentini
#'
#' @examples
#' ## use example data:
#' data("vascEC_meta")
#' data("vascEC_norm")
#' df <- cbind(vascEC_meta,vascEC_norm)
#' res <- makeTDAobj(df, "zone")
#'
#' @export
#'
makeTDAobj <- function(df, outcomes) {

  # checks----------------------------------------------------------------------
  # check missing arguments

  if (missing(df))
    stop("'df' argument must be provided")

  if (missing(outcomes))
    stop("'outcomes' argument must be provided")

  # check the type of argument
  if (!is.data.frame(df))
    stop("'df' argument must be a data.frame")

  if (!is.character(outcomes))
    stop("'outcomes' argument must be a string")

  ## Convert characters to factors
  df <- as.data.frame(unclass(df),
                      stringsAsFactors = TRUE,
                      row.names = rownames(df))
  # specific checks
  if (nrow(df) <= 10)
    stop("The n. of 'df' rows must be greater than 10")

  if (ncol(df) < 2)
    stop("The n. of 'df' columns must be greater than 2")

  if (!all(outcomes %in% colnames(df))) {
    stop("'outcomes' must be one of the 'colnames(df)' ")
  }

  if (any(duplicated(outcomes)))
    stop("duplicated 'outcomes' are not allowed")

  if (length(which(colnames(df) %in% outcomes)) == ncol(df))
    stop("all 'df' columns set as outcomes. Please, consider less outcomes")


  # check the presence of NA or Inf
  if (any(is.na(df)))
    stop("NA values in 'df' are not allowed")

  if (any(is.infinite(as.matrix(df))))
    stop("Inf values in 'df'are not allowed")

  # 'df' splitting
  indNumInt <- which(vapply(df, is.numeric,logical(1)) |
                       vapply(df, is.integer,logical(1))
                     )

  indFact <- which(!(vapply(df, is.numeric,logical(1)) |
                       vapply(df, is.integer,logical(1)))
                   )

  ## handle numeric feats
  if(length(indNumInt) == 0){
    message("There are no numeric variables in 'df' \n")
    df_int <- c()
    dfStd_int <- c()
    dfScaled_int <- c()

  }else{
    df_int <- df[, indNumInt, drop=FALSE]

    # standardizing (z-score)
    dfStd_int <- as.data.frame(scale(df_int,
                                     center = TRUE,
                                     scale = TRUE)
    )
    # scaling (0-1)
    dfScaled_int <- df_int
    for(i in seq_len(ncol(df_int))){
      dfScaled_int[,i] <- scaleData_01(df_int[, i])
    }
  }

  ## handle factorial feats
  if(length(indFact) != 0){
    df_fact <- df[, indFact, drop=FALSE]
    df_fact_num <- as.data.frame(vapply(df_fact,
                                        as.numeric,
                                        numeric(nrow(df_fact)))) -1

    dfScaled_fact_num <- df_fact_num
    dfStd_fact_num <- df_fact_num
    for (i in seq_len(ncol(df_fact_num))){

      if(max(df_fact_num[, i]) > 1){ # if categorical
        dfScaled_fact_num[, i] <- scaleData_01(df_fact_num[, i])
      }
      dfStd_fact_num[, i] <-  as.data.frame(scale(df_fact_num[, i],
                                                 center = TRUE,
                                                 scale = TRUE))
    }
    colnames(dfStd_fact_num) <- colnames(df_fact_num)

    df_new <- cbind(df_fact_num, df_int)
    dfScaled01 <- cbind(dfScaled_fact_num, dfScaled_int)
    dfStdized <- cbind(dfStd_fact_num, dfStd_int)

  }else{
    df_fact <- c()
    message("There are no factorial variables in 'df' \n")

    df_new <- df_int
    dfScaled01 <-  dfScaled_int
    dfStdized <- dfStd_int
  }

  df_new_out <- df_new[, which(colnames(df_new) %in% outcomes), drop=FALSE]

  dfPred <- df_new[, which(colnames(df_new) %!in% outcomes), drop=FALSE]
  dfPredScaled01 <- dfScaled01[, which(colnames(dfScaled01) %!in% outcomes),
                               drop=FALSE]

  PIUMA_tda_object <- new("TDAobj",
                          orig_data = dfPred,
                          scaled_data = dfPredScaled01,
                          outcomeFact = df[, which(colnames(df) %in%
                                                     outcomes), drop = FALSE],
                          outcome = df_new_out)

  return(PIUMA_tda_object)
}
