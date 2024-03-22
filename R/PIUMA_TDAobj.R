#' The object 'TDAobj'
#'
#' The TDA object for storing TDA data
#'
#' @name TDAobj-class
#' @rdname TDAobj-class
#' @docType class
#' @slot orig_data A data.frame of original data (without outcomes)
#' @slot scaled_data A data.frame of re-scaled data (without outcomes)
#' @slot outcomeFact A data.frame of original outcomes
#' @slot outcome A data.frame of original outcomes converted as numeric
#' @slot comp A data.frame containing the components of projected data
#' @slot dist_mat A data.frame containing the computed distance matrix
#' @slot dfMapper A data.frame containing the nodes, with their elements,
#' identified by TDA
#' @slot jacc A matrix of Jaccard indexes between each pair of dfMapper nodes
#' @slot node_data_mat A data.frame with the node size and the average value
#' of each feature
#' @return TDAobj class
#' showClass("TDAobj")
#' @exportClass TDAobj

setClass("TDAobj",
         slots = list(orig_data = "data.frame",
                      scaled_data = "data.frame",
                      outcomeFact = "data.frame",
                      outcome = "data.frame",
                      comp = "data.frame",
                      dist_mat = "data.frame",
                      dfMapper = "data.frame",
                      jacc = "matrix",
                      node_data_mat = "data.frame"
         )
)
