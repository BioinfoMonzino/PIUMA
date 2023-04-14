#' Example datasets for PIUMA package
#'
#' A dataset with metadata extracted from Feng et al.
#' (2022, Nature Communications) to test the
#' \code{\link{importSplitScale}} funtion of \code{PIUMA} package.
#'
#' @format A dataframe containing 1180 rows (cells) and 2 columns (outcomes)
#'
#' @return
#' An example dataset for \code{PIUMA} package
#'
"vascEC_meta"

#' A matrix with scRNA-seq counts data extracted from Feng et al.
#' (2022, Nature Communications) to test the
#' \code{\link{importSplitScale}} funtion of \code{PIUMA} package.
#'
#' @format A matrix containing 1180 rows (cells) and 838 columns (genes)
#'
#' @return
#' An example dataset for \code{PIUMA} package
#'
"vascEC_norm"

#' A dataset to test the \code{\link{dfToProjection}} and
#' \code{\link{dfToDistance}} funtions of \code{PIUMA} package.
#'
#' @format A data.frame containing 15 rows (cells) and 15 columns (genes)
#'
#' @return
#' An example dataset for \code{PIUMA} package
#'
"df_test_proj"

#' A dataset to test the \code{\link{mapperCore}} funtion of \code{PIUMA}
#'  package.
#'
#' @format A data.frame containing 15 rows (cells) and 2 columns (principal
#' components)
#'
#' @return
#' An example dataset for \code{PIUMA} package
#'
"lenses_tda"

#' A dataset to test the \code{\link{mapperCore}} funtion of \code{PIUMA}
#'  package.
#'
#' @format A data.frame containing 15 rows (cells) and 15 columns (cells)
#'
#' @return
#' An example dataset for \code{PIUMA} package
#'
"dist_mat_tda"

#' A dataset to test the \code{\link{jaccardMatrix}} and
#' \code{\link{tdaDfEnrichment}} funtions of \code{PIUMA} package.
#'
#' @format A data.frame containing 15 rows (cells) and 15 columns (genes)
#'
#' @return
#' An example dataset for \code{PIUMA} package
#'
"dfMapper_tda"

#' A dataset to test the \code{\link{checkScaleFreeModel}} funtion
#' of \code{PIUMA} package.
#'
#' @format A data.frame containing 17 rows (nodes) and 17 columns (nodes)
#'
#' @return
#' An example dataset for \code{PIUMA} package
#'
"jacc_mat_tda"
