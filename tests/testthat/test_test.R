########################################################################
## testing dataset
set.seed(1)
samples_num <- 30
var_num <- 5
data_norm_test <- cbind(round(runif(n = samples_num),0),
                        matrix(log2(rnbinom(var_num*samples_num,
                                            mu=150,
                                            size=2)),
                               ncol=var_num)
)
data_norm_test <- as.data.frame(data_norm_test)
colnames(data_norm_test)[1] <- "Y"
data_norm_test$Y <- as.factor(data_norm_test$Y)


############################ import #####################################
test_that("makeTDA_1 works",{
expect_error(makeTDAobj())
expect_error(makeTDAobj(data_norm_test))
expect_error(makeTDAobj(as.matrix(data_norm_test),"Y"))
expect_error(makeTDAobj(data_norm_test,2))
expect_error(makeTDAobj(data_norm_test[1:5,],"Y"))
expect_error(makeTDAobj(data_norm_test[,1],"Y"))
expect_error(makeTDAobj(data_norm_test,colnames(data_norm_test)))
})

outcome_varname <- "Y"
res_import_test <- makeTDAobj(data_norm_test, outcome_varname)

test_that("importSplitScale_2 works",{
# check results
expect_true(dim(res_import_test@outcome)[2] == length(outcome_varname))
expect_true(dim(res_import_test@orig_data)[2] < dim(data_norm_test)[2])
expect_lte(max(res_import_test@scaled_data, na.rm = TRUE),1)
expect_gte(min(res_import_test@scaled_data, na.rm = TRUE),0)
})
############################ distance #####################################

test_that("Distance_1 works",{
#expected error
expect_error(dfToDistance())
expect_no_error(dfToDistance(res_import_test))
expect_error(dfToDistance(res_import_test,"corr"))
expect_error(dfToDistance(as.matrix(res_import_test)))
expect_error(dfToDistance(res_import_test,2))
expect_error(dfToDistance(res_import_test, distMethod=c("euclidean",
                                                           "gower")))
})
res_import_test <- dfToDistance(res_import_test,distMethod = "euclidean")

test_that("Distance_2 works",{
# check results
expect_true(dim(res_import_test@dist_mat)[1] == samples_num)
expect_true(dim(res_import_test@dist_mat)[2] == samples_num)
expect_gte(min(res_import_test@dist_mat),0)
})

############################ projection #####################################

test_that("Projection_1 works",{
#expected error
expect_error(dfToProjection())
expect_no_error(dfToProjection(res_import_test))
expect_error(dfToProjection(res_import_test,"pca"))
expect_error(dfToProjection(as.matrix(res_import_test)))
expect_error(dfToProjection(res_import_test,2))
expect_error(dfToProjection(res_import_test[1:5,]))
expect_error(dfToProjection(res_import_test[,1]))
expect_error(dfToProjection(res_import_test, distMethod=c("PCA", "UMAP")))
expect_error(dfToProjection(res_import_test, "PCA", nComp = 1))
expect_error(dfToProjection(res_import_test, "PCA", nComp = "2"))
expect_error(dfToProjection(res_import_test, "PCA", nComp = 2.5))
expect_error(dfToProjection(res_import_test, "UMAP", umapNNeigh=-2))
expect_error(dfToProjection(res_import_test, "UMAP", umapMinDist = 0))
expect_error(dfToProjection(res_import_test, "KPCA", kpcaKernel = "RBF"))
expect_error(dfToProjection(res_import_test, "TSNE", tsnePerpl = 0.5 ))
expect_error(dfToProjection(res_import_test, "TSNE", tsneMaxIter = 1 ))
})

ncomps=2
res_import_test <- dfToProjection(res_import_test,
                                      "PCA",
                                      nComp=ncomps,
                                      showPlot = FALSE)
test_that("Projection_2 works",{
# check results
expect_true(dim(res_import_test@comp)[1] == samples_num)
expect_true(dim(res_import_test@comp)[2] == ncomps)
})

############################ Mapper #####################################
#expected error

test_that("Mapper_1 works",{
expect_error(mapperCore())
expect_error(mapperCore(dfDistances = res_import_test))
expect_error(mapperCore(df2Dlens = res_import_test))
expect_error(mapperCore(res_import_test,
                        nBins = 0))
expect_error(mapperCore(res_import_test,
                        overlap = 3))
expect_error(mapperCore(res_import_test,
                        mClustNode = "3"))
expect_error(mapperCore(res_import_test,
                        clustMeth = c("kmeans","HR")))
expect_error(mapperCore(res_import_test,
                        res_projection_test,
                        clustMeth = "KMEANS"))
expect_error(mapperCore(res_import_test,
                        HRMethod=c("average","complete")))
expect_error(mapperCore(res_import_test,
                        HRMethod="Avg"))
})
res_import_test<- mapperCore(res_import_test)


test_that("Mapper_1 works",{
# check results
expect_true(dim(res_import_test@dfMapper)[2] == 1)
})

############################ Jaccard #####################################
test_that("jaccard_1 works",{
#expected error
expect_error(jaccardMatrix())
expect_error(jaccardMatrix(7))
expect_error(jaccardMatrix("res_mapper_test"))
})

res_import_test <- jaccardMatrix(res_import_test)

test_that("jaccard_2 works",{
# check results
expect_true(dim(res_import_test@jacc)[2] == dim(res_import_test@dfMapper)[1])
expect_true(dim(res_import_test@jacc)[2] == dim(res_import_test@dfMapper)[1])
expect_lte(max(res_import_test@jacc, na.rm = TRUE),1)
expect_gte(min(res_import_test@jacc, na.rm = TRUE),0)
})




############################ Enrichment #####################################
test_that("Enrichment_1 works",{
expect_error(tdaDfEnrichment())
expect_error(tdaDfEnrichment(res_import_test))
expect_error(tdaDfEnrichment(df = res_import_test@scaled_data))
expect_error(tdaDfEnrichment("res_mapper_test"))
})

res_import_test <-tdaDfEnrichment(res_import_test,
                                  res_import_test@scaled_data)


test_that("Enrichment_2 works",{
# check results
expect_true(dim(res_import_test@node_data_mat)[1] == dim(res_import_test@dfMapper)[1])
expect_true(dim(res_import_test@node_data_mat)[2] == dim(res_import_test@scaled_data)[2]+1)
})


############################ Assessment #####################################
test_that("Entropy_1 work",{
#######Entropy
expect_error(checkNetEntropy())
expect_error(checkNetEntropy(res_import_test))
expect_error(checkNetEntropy("res_import_test"))
})

entropy_net_by_Y <- checkNetEntropy(res_import_test@outcome$Y)

test_that("Entropy_2 work",{
# check results
expect_true(length(entropy_net_by_Y) == 1)
expect_true(is.numeric(entropy_net_by_Y))
})


#######scale-Free
test_that("ScaleFree_1 work",{
expect_error(checkScaleFreeModel())
expect_error(checkScaleFreeModel("res_jacc_test"))
expect_error(checkScaleFreeModel(res_import_test, c("yes","no")))
expect_error(checkScaleFreeModel(res_import_test, "y"))
})


# scalefree_net <- checkScaleFreeModel(res_import_test, "no")
#
# test_that("ScaleFree_2 work",{
# # check results
# expect_true(is.list(scalefree_net))
# expect_true(scalefree_net$pValkpk <= 1)
# expect_true(scalefree_net$pVallogklogpk <= 1)
# expect_true(scalefree_net$corkpk <= 1)
# expect_true(scalefree_net$corlogklogpk <= 1)
#
# expect_true(scalefree_net$pValkpk >= 0)
# expect_true(scalefree_net$pVallogklogpk >= 0)
# expect_true(scalefree_net$corkpk >= -1)
# expect_true(scalefree_net$corlogklogpk >= -1)
#
# })












