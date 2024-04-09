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
res_import_test <- mapperCore(res_import_test)
res_import_test <- jaccardMatrix(res_import_test)
res_import_test <- tdaDfEnrichment(res_import_test,
                                   getScaledData(res_import_test))

#######Entropy
test_that("Entropy_1 work",{
  expect_error(checkNetEntropy())
  expect_error(checkNetEntropy(res_import_test))
  expect_error(checkNetEntropy("res_import_test"))
})

entropy_net_by_Y <- checkNetEntropy(getOutcome(res_import_test)$Y)

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

