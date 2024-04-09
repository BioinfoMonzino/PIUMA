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

# check arguments
test_that("Distance_1 works",{
  expect_error(dfToDistance())
  expect_no_error(dfToDistance(res_import_test))
  expect_error(dfToDistance(res_import_test,"corr"))
  expect_error(dfToDistance(as.matrix(res_import_test)))
  expect_error(dfToDistance(res_import_test,2))
  expect_error(dfToDistance(res_import_test, distMethod=c("euclidean",
                                                          "gower")))
})
res_import_test <- dfToDistance(res_import_test,distMethod = "euclidean")

# check results
test_that("Distance_2 works",{
  expect_true(nrow(getDistMat(res_import_test)) == samples_num)
  expect_true(ncol(getDistMat(res_import_test)) == samples_num)
  expect_gte(min(getDistMat(res_import_test)),0)
})
