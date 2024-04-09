#' @title Getter method for the 'orig_data' slot of a TDAobj object.
#'
#' @description The method to get data from the orig_data slot
#'
#' @docType methods
#' @rdname getOrigData
#' @aliases getOrigData getOrigData,PIUMA-getOrigData
#' @param x a \code{TDAobj} object
#' @return a data.frame with the original data
#'
#' @author Mattia Chiesa
#'
#' @examples
#' data(tda_test_data)
#' ex_out <- getOrigData(tda_test_data)
#' @export
setMethod("getOrigData",
          c(x="TDAobj"),
          function(x){
              out_df <- x@orig_data
              return(out_df)
          })

#' @title Setter method for the 'orig_data' slot of a TDAobj object.
#'
#' @description The method to set the orig_data slot
#'
#' @docType methods
#' @rdname setOrigData
#' @aliases setOrigData setOrigData,PIUMA-setOrigData
#' @param x a \code{TDAobj} object
#' @param y a data.frame with the original data
#' @return a \code{TDAobj} object
#'
#' @author Mattia Chiesa
#'
#' @examples
#' data(tda_test_data)
#' @export
setMethod("setOrigData",
          c(x = "TDAobj"),
          function(x, y){
              x@orig_data <- y
              return(x)
          })

#' @title Getter method for the 'scaled_data' slot of a TDAobj object.
#'
#' @description The method to get data from the scaled_data slot
#'
#' @docType methods
#' @rdname getScaledData
#' @aliases getScaledData getScaledData,PIUMA-getScaledData
#' @param x a \code{TDAobj} object
#' @return a data.frame with the scaled data
#'
#' @author Mattia Chiesa
#'
#' @examples
#' data(tda_test_data)
#' ex_out <- getScaledData(tda_test_data)
#' @export
setMethod("getScaledData",
          c(x = "TDAobj"),
          function(x){
              out_df <- x@scaled_data
              return(out_df)
          })

#' @title Setter method for the 'scaled_data' slot of a TDAobj object.
#'
#' @description The method to set the scaled_data slot
#'
#' @docType methods
#' @rdname setScaledData
#' @aliases setScaledData setScaledData,PIUMA-setScaledData
#' @param x a \code{TDAobj} object
#' @param y a data.frame with the scaled data
#' @return a \code{TDAobj} object
#'
#' @author Mattia Chiesa
#'
#' @examples
#' data(tda_test_data)
#' @export
setMethod("setScaledData",
          c(x = "TDAobj"),
          function(x,y){
              x@scaled_data <- y
              return(x)
          })

#' @title Getter method for the 'outcomeFact' slot of a TDAobj object.
#'
#' @description The method to get data from the outcomeFact slot
#'
#' @docType methods
#' @rdname getOutcomeFact
#' @aliases getOutcomeFact getOutcomeFact,PIUMA-getOutcomeFact
#' @param x a \code{TDAobj} object
#' @return a data.frame with the outcomeFact data
#'
#' @author Mattia Chiesa
#'
#' @examples
#' data(tda_test_data)
#' ex_out <- getOutcomeFact(tda_test_data)
#' @export
setMethod("getOutcomeFact",
          c(x = "TDAobj"),
          function(x){
              out_df <- x@outcomeFact
              return(out_df)
          })

#' @title Setter method for the 'outcomeFact' slot of a TDAobj object.
#'
#' @description The method to set the outcomeFact slot
#'
#' @docType methods
#' @rdname setOutcomeFact
#' @aliases setOutcomeFact setOutcomeFact,PIUMA-setOutcomeFact
#' @param x a \code{TDAobj} object
#' @param y a data.frame with the outcomeFact data
#' @return a \code{TDAobj} object
#'
#' @author Mattia Chiesa
#'
#' @examples
#' data(tda_test_data)
#' @export
setMethod("setOutcomeFact",
          c(x = "TDAobj"),
          function(x,y){
              x@outcomeFact <- y
              return(x)
          })

#' @title Getter method for the 'outcome' slot of a TDAobj object.
#'
#' @description The method to get data from the outcome slot
#'
#' @docType methods
#' @rdname getOutcome
#' @aliases getOutcome getOutcome,PIUMA-getOutcome
#' @param x a \code{TDAobj} object
#' @return a data.frame with the outcome data
#'
#' @author Mattia Chiesa
#'
#' @examples
#' data(tda_test_data)
#' ex_out <- getOutcome(tda_test_data)
#' @export
setMethod("getOutcome",
          c(x = "TDAobj"),
          function(x){
              out_df <- x@outcome
              return(out_df)
          })

#' @title Setter method for the 'outcome' slot of a TDAobj object.
#'
#' @description The method to set the outcome slot
#'
#' @docType methods
#' @rdname setOutcome
#' @aliases setOutcome setOutcome,PIUMA-setOutcome
#' @param x a \code{TDAobj} object
#' @param y a data.frame with the outcome data
#' @return a \code{TDAobj} object
#'
#' @author Mattia Chiesa
#'
#' @examples
#' data(tda_test_data)
#' @export
setMethod("setOutcome",
          c(x = "TDAobj"),
          function(x,y){
              x@outcome <- y
              return(x)
          })

#' @title Getter method for the 'comp' slot of a TDAobj object.
#'
#' @description The method to get data from the comp slot
#'
#' @docType methods
#'
#' @rdname getComp
#' @aliases getComp getComp,PIUMA-getComp
#' @param x a \code{TDAobj} object
#' @return a data.frame with the comp data
#'
#' @author Mattia Chiesa
#'
#' @examples
#' data(tda_test_data)
#' @export
setMethod("getComp",
          c(x = "TDAobj"),
          function(x){
              out_df <- x@comp
              return(out_df)
          })

#' @title Setter method for the 'comp' slot of a TDAobj object.
#'
#' @description The method to set the comp slot
#'
#' @docType methods
#' @rdname setComp
#' @aliases setComp setComp,PIUMA-setComp
#' @param x a \code{TDAobj} object
#' @param y a data.frame with the comp data
#' @return a \code{TDAobj} object
#'
#' @author Mattia Chiesa
#'
#' @examples
#' data(tda_test_data)
#' @export
setMethod("setComp",
          c(x = "TDAobj"),
          function(x,y){
              x@comp <- y
              return(x)
          })

#' @title Getter method for the 'dist_mat' slot of a TDAobj object.
#'
#' @description The method to get data from the dist_mat slot
#'
#' @docType methods
#' @rdname getDistMat
#' @aliases getDistMat getDistMat,PIUMA-getDistMat
#' @param x a \code{TDAobj} object
#' @return a data.frame with the dist_mat data
#'
#' @author Mattia Chiesa
#'
#' @examples
#' data(tda_test_data)
#' ex_out <- getDistMat(tda_test_data)
#' @export
setMethod("getDistMat",
          c(x = "TDAobj"),
          function(x){
              out_df <- x@dist_mat
              return(out_df)
          })

#' @title Setter method for the 'dist_mat' slot of a TDAobj object.
#'
#' @description The method to set the dist_mat slot
#'
#' @docType methods
#' @rdname setDistMat
#' @aliases setDistMat setDistMat,PIUMA-setDistMat
#' @param x a \code{TDAobj} object
#' @param y a data.frame with the dist_mat data
#' @return a \code{TDAobj} object
#'
#' @author Mattia Chiesa
#'
#' @examples
#' data(tda_test_data)
#' @export
setMethod("setDistMat",
          c(x = "TDAobj"),
          function(x,y){
              x@dist_mat <- y
              return(x)
          })

#' @title Getter method for the 'dfMapper' slot of a TDAobj object.
#'
#' @description The method to get data from the dfMapper slot
#'
#' @docType methods
#' @rdname getDfMapper
#' @aliases getDfMapper getDfMapper,PIUMA-getDfMapper
#' @param x a \code{TDAobj} object
#' @return a data.frame with the dfMapper data
#'
#' @author Mattia Chiesa
#'
#' @examples
#' data(tda_test_data)
#' ex_out <- getDfMapper(tda_test_data)
#' @export
setMethod("getDfMapper",
          c(x = "TDAobj"),
          function(x){
              out_df <- x@dfMapper
              return(out_df)
          })

#' @title Setter method for the 'dfMapper' slot of a TDAobj object.
#'
#' @description The method to set the dfMapper slot
#'
#' @docType methods
#' @rdname setDfMapper
#' @aliases setDfMapper setDfMapper,PIUMA-setDfMapper
#' @param x a \code{TDAobj} object
#' @param y a data.frame with the dfMapper data
#' @return a \code{TDAobj} object
#'
#' @author Mattia Chiesa
#'
#' @examples
#' data(tda_test_data)
#' @export
setMethod("setDfMapper",
          c(x = "TDAobj"),
          function(x,y){
              x@dfMapper <- y
              return(x)
          })

#' @title Getter method for the 'jacc' slot of a TDAobj object.
#'
#' @description The method to get data from the jacc slot
#'
#' @docType methods
#' @rdname getJacc
#' @aliases getJacc getJacc,PIUMA-getJacc
#' @param x a \code{TDAobj} object
#' @return a matrix with the jacc data
#'
#' @author Mattia Chiesa
#'
#' @examples
#' data(tda_test_data)
#' ex_out <- getJacc(tda_test_data)
#' @export
setMethod("getJacc",
          c(x = "TDAobj"),
          function(x){
              out_df <- x@jacc
              return(out_df)
          })

#' @title Setter method for the 'jacc' slot of a TDAobj object.
#'
#' @description The method to set the jacc slot
#'
#' @docType methods
#' @rdname setJacc
#' @aliases setJacc setJacc,PIUMA-setJacc
#' @param x a \code{TDAobj} object
#' @param y a matrix with the jacc data
#' @return a \code{TDAobj} object
#'
#' @author Mattia Chiesa
#'
#' @examples
#' data(tda_test_data)
#' @export
setMethod("setJacc",
          c(x = "TDAobj"),
          function(x,y){
              x@jacc <- y
              return(x)
          })

#' @title Getter method for the 'node_data_mat' slot of a TDAobj object.
#'
#' @description The method to get data from the node_data_mat slot
#'
#' @docType methods
#' @rdname getNodeDataMat
#' @aliases getNodeDataMat getNodeDataMat,PIUMA-getNodeDataMat
#' @param x a \code{TDAobj} object
#' @return a data.frame with the node_data_mat data
#'
#' @author Mattia Chiesa
#'
#' @examples
#' data(tda_test_data)
#' ex_out <- getNodeDataMat(tda_test_data)
#' @export
setMethod("getNodeDataMat",
          c(x = "TDAobj"),
          function(x){
              out_df <- x@node_data_mat
              return(out_df)
          })

#' @title Setter method for the 'node_data_mat' slot of a TDAobj object.
#'
#' @description The method to set the node_data_mat slot
#'
#' @docType methods
#' @rdname setNodeDataMat
#' @aliases setNodeDataMat setNodeDataMat,PIUMA-setNodeDataMat
#' @param x a \code{TDAobj} object
#' @param y a data.frame with the node_data_mat data
#' @return a \code{TDAobj} object
#'
#' @author Mattia Chiesa
#'
#' @examples
#' data(tda_test_data)
#' @export
setMethod("setNodeDataMat",
          c(x = "TDAobj"),
          function(x,y){
              x@node_data_mat <- y
              return(x)
          })
