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

test_that("jaccard_1 works",{
  expect_error(jaccardMatrix())
  expect_error(jaccardMatrix(7))
  expect_error(jaccardMatrix("res_mapper_test"))
})

res_import_test <- jaccardMatrix(res_import_test)

# check results
test_that("jaccard_2 works",{
  expect_true(ncol(getJacc(res_import_test)) == nrow(getDfMapper(
    res_import_test)))
  expect_lte(max(getJacc(res_import_test), na.rm = TRUE), 1)
  expect_gte(min(getJacc(res_import_test), na.rm = TRUE), 0)
})


