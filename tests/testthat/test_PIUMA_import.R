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

# check arguments
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

# check results
test_that("makeTDA_2 works",{
  expect_true(ncol(getOutcome(res_import_test)) == length(outcome_varname))
  expect_true(ncol(getOrigData(res_import_test)) < ncol(data_norm_test))
  expect_lte(max(getScaledData(res_import_test), na.rm = TRUE),1)
  expect_gte(min(getScaledData(res_import_test), na.rm = TRUE),0)
})
