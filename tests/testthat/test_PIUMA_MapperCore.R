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
res_import_test <- dfToProjection(res_import_test,
                                  "PCA",
                                  nComp=2,
                                  showPlot = FALSE)

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
res_import_test <- mapperCore(res_import_test)

# check results
test_that("Mapper_1 works",{
  expect_true(ncol(getDfMapper(res_import_test)) == 1)
})
