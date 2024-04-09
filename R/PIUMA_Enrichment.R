#' @title Add information to TDAobj
#'
#' @description This function computes the average value of additional features
#' provided by the user and calculate the size for each node of 'dfMapper' slot
#'
#' @param x A TDAobj object, processed by the  \code{\link{mapperCore}}
#' function.
#' @param df A data.frame with scaled values in the classical n x m form:
#' rows (n) and columns (m) must be observations and features, respectively.
#'
#' @return The starting TDAobj object, in which the a data.frame with additional
#'  information for each node has been added (slot: 'node_data_mat')
#' @author Mattia Chiesa, Laura Ballarini, Luca Piacentini
#'
#' @examples
#' ## use example data:
#' data(tda_test_data)
#' data(df_test_proj)
#' enrich_mat_tda <- tdaDfEnrichment(tda_test_data, df_test_proj)
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
tdaDfEnrichment <- function(x, df){

  dfMapper <- getDfMapper(x)
  # checks----------------------------------------------------------------------
  # check missing arguments
  if (missing(dfMapper))
    stop("'dfMapper' argument must be provided")

  if (missing(df))
    stop("'df' argument must be provided")

  # check the type of argument
  if (!is.data.frame(dfMapper))
    stop("'dfMapper' argument must be a data.frame")

  if (!is.data.frame(df))
    stop("'df' argument must be a data.frame")

  # specific checks
  if (ncol(dfMapper) != 1)
    stop("ncol(dfMapper) must be equal to 1")

  if (!(all(vapply(dfMapper, is.character,logical(1)))))
    stop("'dfMapper' variables must be numeric")

  if (!(all(vapply(df, is.numeric,logical(1))) |
        all(vapply(df, is.integer,logical(1))))
  )
    stop("'df' variables must be numeric")


  # check the presence of NA or Inf
  if (any(is.na(dfMapper)))
    stop("NA values are not allowed in the 'dfMapper' data.frame")

  if (any(is.infinite(as.matrix(dfMapper))))
    stop("Inf values are not allowed in the 'dfMapper' data.frame")

  if (any(is.na(df)))
    stop("NA values are not allowed in the 'df' data.frame")

  if (any(is.infinite(as.matrix(df))))
    stop("Inf values are not allowed in the 'df' data.frame")

  # body--------------------------------
  dbDataRes <- matrix(nrow = nrow(dfMapper), ncol=ncol(df))
  colnames(dbDataRes) <- colnames(df)
  rownames(dbDataRes) <- rownames(dfMapper)

  nodeSize <- c()
  for (i in seq_len(nrow(dfMapper))) {
    listaSample <- unlist(strsplit(dfMapper[i, 1], " "))
    # compute the means for each variable
    vectorMeans <- round(colMeans(df[listaSample,, drop=FALSE],
                                  na.rm = TRUE), 3)
    dbDataRes[i,] <- vectorMeans
    nodeSize[i] <- length(listaSample)
  }
  dbDataRes <- as.data.frame(dbDataRes)
  # add a column with the number of examples for each node
  dbDataRes$size <- nodeSize

  x <- setNodeDataMat(x, dbDataRes)
  return(x)
}
