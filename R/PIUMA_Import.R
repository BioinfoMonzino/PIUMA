#' @title Import and handle dataframes for TDA Mapper
#'
#' @description This function import a data.frame and performs some preliminary
#' steps on it. specifically, outcomes variables data will be separated by
#' the rest of data. The remaining dataset will be also re-scaled (0-1) and
#' standardized (z-score).
#'
#' @param df A data.frame representing a dataset in the classical n x m form.
#' Rows (n) and columns (m) should be, respectively, observations and features.
#' @param outcomes A string or vector of string containing the name of variables
#' that have to be considered 'outcomes'
#'
#' @return A list containing:
#' \itemize{
#'   \item A data.frame of original data (without outcomes).
#'   \item A data.frame of re-scaled data (without outcomes)
#'   \item A data.frame of original outcomes.
#'   \item A data.frame of original outcomes converted as numeric.
#' }
#'
#' @author Laura Ballarini, Mattia Chiesa
#'
#' @examples
#' ## use example data:
#' data("vascEC_meta")
#' data("vascEC_norm")
#' df <- cbind(vascEC_meta,vascEC_norm)
#' res <- importSplitScale(df, "zone")
#'
#' @export
#'
importSplitScale <- function(df, outcomes) {

  # inner function to standardize data [0; 1]-----------------------------------
  scaleData_01 <- function(x) {
    return((x-min(x))/(max(x)-min(x)))
  }

  '%!in%' <- function(x,y)!('%in%'(x,y))



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
                      stringsAsFactors=TRUE,
                      row.names = rownames(df))
  # specific checks
  if (dim(df)[1] <= 10)
    stop("The n. of 'df' rows must be greater than 10")

  if (dim(df)[2] < 2)
    stop("The n. of 'df' columns must be greater than 2")

  if (!all(outcomes %in% colnames(df))) {
    stop("'outcomes' must be one of the 'colnames(df)' ")
  }

  if (any(duplicated(outcomes)))
    stop("duplicated 'outcomes' are not allowed")

  if (length(which(colnames(df) %in% outcomes)) == dim(df)[2])
    stop("all 'df' columns set as outcomes. Please, consider less outcomes")


  # check the presence of NA or Inf
  if (any(is.na(df)))
    stop("NA values in 'df' are not allowed")

  if (any(is.infinite(as.matrix(df))))
    stop("Inf values in 'df'are not allowed")

  # 'df' splitting (body)-------------------------------------------------------

  # find numeric columns
  indNumInt <- which(vapply(df, class, FUN.VALUE=character(1)) %in%
                       c("numeric", "integer"))

  # find factor columns
  indFact<-which(vapply(df, class, FUN.VALUE=character(1)) %!in%
                   c("numeric", "integer"))

  ## handle numeric feats
  if(length(indNumInt) == 0){
    cat("There are no numeric variables in 'df' \n")
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
      dfScaled_int[,i] <- scaleData_01(df_int[,i])
    }
  }

  ## handle factorial feats
  if(length(indFact) !=0){
#  cat("Binary variables will be converted to 0/1.
#  Categorical var will be converted to integer based on alphabetic order")

    df_fact <- df[, indFact, drop=FALSE]
    df_fact_num <- as.data.frame(sapply(df_fact, as.numeric)) -1

    dfScaled_fact_num <- dfStd_fact_num <- df_fact_num
    for (i in seq_len(ncol(df_fact_num))){

      if(max(df_fact_num[,i]) > 1){ # if categorical

        dfScaled_fact_num[,i] <- scaleData_01(df_fact_num[,i])

      }

      dfStd_fact_num[,i] <-  as.data.frame(scale(df_fact_num[,i],
                                       center = TRUE,
                                       scale = TRUE))
    }
    colnames(dfStd_fact_num) <- colnames(df_fact_num)

    df_new <- cbind(df_fact_num, df_int)
    dfScaled01 <- cbind(dfScaled_fact_num, dfScaled_int)
    dfStdized <- cbind(dfStd_fact_num, dfStd_int)

  }else{
    df_fact <- c()
    cat("There are no factorial variables in 'df' \n")

    df_new <- df_int
    dfScaled01 <-  dfScaled_int
    dfStdized <- dfStd_int
  }

  df_new_out <- df_new[,which(colnames(df_new) %in% outcomes), drop=FALSE]

  dfPred <- df_new[,which(colnames(df_new) %!in% outcomes), drop=FALSE]
  dfPredScaled01 <- dfScaled01[,which(colnames(dfScaled01) %!in% outcomes),
                               drop=FALSE]
  dfPredStdized <- dfScaled01[,which(colnames(dfStdized) %!in% outcomes),
                              drop=FALSE]

  # add the four created dataframes to a list
  retDfList <- list()
  retDfList[['data_real']] <- dfPred
  retDfList[['data_scaled01']] <- dfPredScaled01
  #retDfList[['data_stdized']] <- dfPredStdized
  retDfList[['outcome']] <- df_new_out
  
  if(length(indFact) !=0){
  retDfList[['outcomeFact']] <- df[,which(colnames(df) %in% outcomes),
                                   drop=FALSE]
  }
  return (retDfList)
}
