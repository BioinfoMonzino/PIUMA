#' Example datasets for PIUMA package
#'
#' We tested PIUMA on a subset of the single-cell RNA Sequencing dataset
#' (GSE:GSE193346 generated and published by Feng et al. (2022)
#' on Nature Communication to demonstrate that distinct
#' transcriptional profiles are present in specific cell types of each heart
#' chambers, which were attributed to have roles in cardiac development.
#' In this tutorial, our aim will be to exploit PIUMA for identifying
#' sub-population of vascular endothelial cells, which can be associated
#' with specific heart developmental stages. The original dataset consisted
#' of three layers of heterogeneity: cell type, stage and zone
#' (i.e., heart chamber). Our testing dataset was obtained by
#' subsetting vascular endothelial cells (cell type) by Seurat object,
#' extracting raw counts and metadata. Thus, we filtered low expressed genes
#' and normalized data by DaMiRseq
#'
#' @format A dataframe containing 1180 rows (cells) and 2 columns (outcomes)
#'
#' @return
#' An example dataset for \code{PIUMA} package
#'
"vascEC_meta"

#' We tested PIUMA on a subset of the single-cell RNA Sequencing dataset
#' (GSE:GSE193346 generated and published by Feng et al. (2022)
#' on Nature Communication to demonstrate that distinct
#' transcriptional profiles are present in specific cell types of each heart
#' chambers, which were attributed to have roles in cardiac development.
#' In this tutorial, our aim will be to exploit PIUMA for identifying
#' sub-population of vascular endothelial cells, which can be associated
#' with specific heart developmental stages. The original dataset consisted
#' of three layers of heterogeneity: cell type, stage and zone
#' (i.e., heart chamber). Our testing dataset was obtained by
#' subsetting vascular endothelial cells (cell type) by Seurat object,
#' extracting raw counts and metadata. Thus, we filtered low expressed genes
#' and normalized data by DaMiRseq
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

#' A TDAobj to test the \code{PIUMA} package.
#'
#' @format A TDAobj with data in all slots
#'
#' @return
#' An example dataset for \code{PIUMA} package
"tda_test_data"
