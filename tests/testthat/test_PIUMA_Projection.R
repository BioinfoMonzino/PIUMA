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
outcome_varname <- "Y"
res_import_test <- makeTDAobj(data_norm_test, outcome_varname)
res_import_test <- dfToDistance(res_import_test,distMethod = "euclidean")

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
  expect_true(nrow(getComp(res_import_test)) == samples_num)
  expect_true(ncol(getComp(res_import_test)) == ncomps)
})


