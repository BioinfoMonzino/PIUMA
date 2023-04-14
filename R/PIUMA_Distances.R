#' @title Compute the Distance Matrix from a dataset
#'
#' @description This function returns the distance matrix computed by using the
#'  Pearson's, Euclidean or Gower distance methods. The distances are
#' computed between the rows of a data.frame in the classical form n x m,
#'  where n (rows) are observations and m (columns) are features.
#'
#' @param df A data.frame representing a dataset in the classical n x m form.
#' Rows (n) and columns (m) should be, respectively, observations and features.
#' @param distMethod The distance method to calculate the distance matix.
#' "euclidean", "gower" and "pearson" values are allowed. Default: "euclidean".
#'
#' @return A data.frame containing the distance matrix computed
#'
#' @author Laura Ballarini, Mattia Chiesa
#'
#' @examples
#' ## use example data:
#' data(df_test_proj)
#' dfDist <- dfToDistance(df_test_proj, "euclidean")
#'
#' @seealso
#' \code{\link{importSplitScale}}
#'
#' @export
#'
dfToDistance<-function(df, distMethod=c("euclidean", "gower", "pearson")) {

  # checks----------------------------------------------------------------------
  # check missing arguments
  if (missing(df))
    stop("'df' argument must be provided")

  if (missing(distMethod)){
    distMethod <- distMethod[1]
  }


  # check the type of argument
  if (!is.data.frame(df))
    stop("'df' argument must be a data.frame")

  if (!is.character(distMethod))
    stop("'distMethod' argument must be character")


  # specific checks
  if (dim(df)[1] < 10)
    stop("n. of 'df' row must be greater than 10")

  if (dim(df)[2] < 2)
    stop("n. of 'df'columns must be greater than 2")

  if (length(distMethod) > 1)
    stop("length(distMethod) must be equal to 1")

  if (!(distMethod %in% c("euclidean", "gower", "pearson")))
    stop("'distMethod' must be one of 'euclidean', 'gower', 'pearson'")


  # other more specific 'df' contents checks
  if (isSymmetric.matrix(as.matrix(df))) {
    if (sum(diag(as.matrix(df))) == 0 & min(df) >= 0) {
      warning("you choose 'data' as 'dfType', but 'df' seems to be a 'distance
      matrix' as it is symmetric with all diagonal elements == 0 and with
      non-negative elements (dissimilarity). Please check it!")
    } else if (all(diag(as.matrix(df)) == 1) & min(df) >= 0) {
      warning("you choose 'data' as 'dfType', but 'df' seems to be a 'distance
      matrix' as it is symmetric with trace == 1 and with non-negative elements
      (similarity). Please check it")
    } else if (sum(diag(as.matrix(df))) == 0 & min(df) < 0) {
      warning("'df' is symmetric with all diagonal elements == 0 and
      without non-negative elements. Please check 'df' elements and 'dfType'")
    } else if (all(diag(as.matrix(df)) == 1) & min(df) < 0) {
      warning("you choose 'data' as 'dfType', but 'df' seems to be a 'distance
      matrix' as it is symmetric with all diagonal elements == 1 and without
      non-negative elements. Please check 'df' elements and 'dfType'")
    } else {
      warning("the matrix corresponding to 'df' is symmetric, without  all
              diagonal elements == 0 or == 1. Please check your 'df' elements
              and 'dfType'")
    }
  }

  if ((!all(sapply(df, class) %in% c("numeric", "integer"))))
    stop("'df' variables must be numeric")

  # check the presence of NA or Inf
  if (any(is.na(df)))
    stop("NA values are not allowed in the 'df' data.frame")

  if (any(is.infinite(as.matrix(df))))
    stop("Inf values are not allowed in the 'df' data.frame")


  # distance computation (body)-------------------------------------------------
  switch(distMethod, "euclidean"={

    distance<-dist(df, method=distMethod)
    dfDist<-as.data.frame(as.matrix(distance))

  }, "gower"={

    dfDist <- as.data.frame(as.matrix(daisy(df, metric = "gower",
                                            stand = FALSE, warnType = FALSE)))

  }, "pearson"={

    dfDist <- as.data.frame(1-cor(t(df)), method="pearson")

  }
  )

  return(dfDist)
}
