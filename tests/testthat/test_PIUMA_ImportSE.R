data("vascEC_meta")
data("vascEC_norm")
suppressMessages(library(SummarizedExperiment))
dataSE <- SummarizedExperiment(assays=as.matrix(t(vascEC_norm)),
                               colData=as.data.frame(vascEC_meta))


# check arguments
test_that("makeTDAFromSE_1 works",{
  expect_error(makeTDAobjFromSE())
  expect_error(makeTDAobjFromSE(dataSE))
  expect_error(makeTDAobj(as.matrix(dataSE),"Y"))
  expect_error(makeTDAobj(dataSE,2))
  expect_error(makeTDAobj(dataSE[1:5,],"Y"))
  expect_error(makeTDAobj(dataSE[,1],"Y"))
  expect_error(makeTDAobj(dataSE,colData(dataSE)))
})

res_import_test <- makeTDAobjFromSE(dataSE, colnames(colData(dataSE)))

# check results
test_that("makeTDAFromSE_2 works",{
  expect_true(ncol(getOutcome(res_import_test)) >= 1)
})
