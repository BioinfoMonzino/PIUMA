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
expect_error(importSplitScale())
expect_error(importSplitScale(data_norm_test))
expect_error(importSplitScale(as.matrix(data_norm_test),"Y"))
expect_error(importSplitScale(data_norm_test,2))
expect_error(importSplitScale(data_norm_test[1:5,],"Y"))
expect_error(importSplitScale(data_norm_test[,1],"Y"))
expect_error(importSplitScale(data_norm_test,colnames(data_norm_test)))


outcome_varname <- "Y"
res_import_test <- importSplitScale(data_norm_test, outcome_varname)

# check results
expect_true(dim(res_import_test$outcome)[2] == length(outcome_varname))
expect_true(dim(res_import_test$data_real)[2] < dim(data_norm_test)[2])
expect_lte(max(res_import_test$data_scaled01, na.rm = TRUE),1)
expect_gte(min(res_import_test$data_scaled01, na.rm = TRUE),0)


############################ distance #####################################
data_distance_test <- res_import_test$data_scaled01
#expected error
expect_error(dfToDistance())
expect_no_error(dfToDistance(data_distance_test))
expect_error(dfToDistance(data_distance_test,"corr"))
expect_error(dfToDistance(as.matrix(data_distance_test)))
expect_error(dfToDistance(data_distance_test,2))
expect_error(dfToDistance(data_distance_test[1:5,]))
expect_error(dfToDistance(data_distance_test[,1]))
expect_error(dfToDistance(data_distance_test, distMethod=c("euclidean",
                                                           "gower")))


res_distance_test <- dfToDistance(data_distance_test,distMethod = "euclidean")

# check results
expect_true(dim(res_distance_test)[1] == dim(data_distance_test)[1])
expect_true(dim(res_distance_test)[2] == dim(data_distance_test)[1])
expect_gte(min(res_distance_test),0)


############################ projection #####################################
#expected error
expect_error(dfToProjection())
expect_no_error(dfToProjection(data_distance_test))
expect_error(dfToProjection(data_distance_test,"pca"))
expect_error(dfToProjection(as.matrix(data_distance_test)))
expect_error(dfToProjection(data_distance_test,2))
expect_error(dfToProjection(data_distance_test[1:5,]))
expect_error(dfToProjection(data_distance_test[,1]))
expect_error(dfToProjection(data_distance_test, distMethod=c("PCA", "UMAP")))
expect_error(dfToProjection(data_distance_test, "PCA", nComp = 1))
expect_error(dfToProjection(data_distance_test, "PCA", nComp = "2"))
expect_error(dfToProjection(data_distance_test, "PCA", nComp = 2.5))
expect_error(dfToProjection(data_distance_test, "UMAP", umapNNeigh=-2))
expect_error(dfToProjection(data_distance_test, "UMAP", umapMinDist = 0))
expect_error(dfToProjection(data_distance_test, "KPCA", kpcaKernel = "RBF"))
expect_error(dfToProjection(data_distance_test, "TSNE", tsnePerpl = 0.5 ))
expect_error(dfToProjection(data_distance_test, "TSNE", tsneMaxIter = 1 ))

ncomps=2
res_projection_test <- dfToProjection(data_distance_test,
                                      "PCA",
                                      nComp=ncomps,
                                      showPlot = FALSE)

# check results
expect_true(dim(res_projection_test)[1] == dim(data_distance_test)[1])
expect_true(dim(res_projection_test)[2] == ncomps)

############################ Mapper #####################################
#expected error
expect_error(mapperCore())
expect_error(mapperCore(dfDistances = res_distance_test))
expect_error(mapperCore(df2Dlens = res_projection_test))
expect_error(mapperCore(res_distance_test,
                        res_projection_test,
                        nBins = 0))
expect_error(mapperCore(res_distance_test,
                        res_projection_test,
                        overlap = 3))
expect_error(mapperCore(res_distance_test,
                        res_projection_test,
                        mClustNode = "3"))
expect_error(mapperCore(res_distance_test,
                        res_projection_test,
                        clustMeth = c("kmeans","HR")))
expect_error(mapperCore(res_distance_test,
                        res_projection_test,
                        clustMeth = "KMEANS"))
expect_error(mapperCore(res_distance_test,
                        res_projection_test,
                        HRMethod=c("average","complete")))
expect_error(mapperCore(res_distance_test,
                        res_projection_test,
                        HRMethod="Avg"))

res_mapper_test<- mapperCore(res_distance_test, res_projection_test)

# check results
expect_true(dim(res_mapper_test)[2] == 1)


############################ Jaccard #####################################
#expected error
expect_error(jaccardMatrix())
expect_error(jaccardMatrix(res_distance_test))
expect_error(jaccardMatrix("res_mapper_test"))

res_jacc_test <- jaccardMatrix(res_mapper_test)

# check results
expect_true(dim(res_jacc_test)[2] == dim(res_mapper_test)[1])
expect_true(dim(res_jacc_test)[2] == dim(res_mapper_test)[1])
expect_lte(max(res_jacc_test, na.rm = TRUE),1)
expect_gte(min(res_jacc_test, na.rm = TRUE),0)





############################ Enrichment #####################################
expect_error(tdaDfEnrichment())
expect_error(tdaDfEnrichment(res_mapper_test))
expect_error(tdaDfEnrichment(df = res_import_test$data_scaled01))
expect_error(tdaDfEnrichment("res_mapper_test"))

res_tdaEnr_test <-tdaDfEnrichment(res_mapper_test,
                                  res_import_test$data_scaled01)
# check results
expect_true(dim(res_tdaEnr_test)[1] == dim(res_mapper_test)[1])
expect_true(dim(res_tdaEnr_test)[2] == dim(res_import_test$data_scaled01)[2]+1)



############################ Assessment #####################################
#######Entropy
expect_error(checkNetEntropy())
expect_error(checkNetEntropy(res_tdaEnr_test))
expect_error(checkNetEntropy("res_tdaEnr_test"))

entropy_net_by_v2 <- checkNetEntropy(res_tdaEnr_test$V2)

# check results
expect_true(length(entropy_net_by_v2) == 1)
expect_true(is.numeric(entropy_net_by_v2))

#######scale-Free
expect_error(checkScaleFreeModel())
expect_error(checkScaleFreeModel("res_jacc_test"))
expect_error(checkScaleFreeModel(res_jacc_test, c("yes","no")))
expect_error(checkScaleFreeModel(res_jacc_test, "y"))

scalefree_net <- checkScaleFreeModel(res_jacc_test, "no")

# check results
expect_true(is.list(scalefree_net))
expect_true(scalefree_net$pValkpk <= 1)
expect_true(scalefree_net$pVallogklogpk <= 1)
expect_true(scalefree_net$corkpk <= 1)
expect_true(scalefree_net$corlogklogpk <= 1)

expect_true(scalefree_net$pValkpk >= 0)
expect_true(scalefree_net$pVallogklogpk >= 0)
expect_true(scalefree_net$corkpk >= -1)
expect_true(scalefree_net$corlogklogpk >= -1)














