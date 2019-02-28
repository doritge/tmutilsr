context("lda_expose_docs")
library(tmutilsr)

test_that("number of returned docs is correct", {
    expect_equal(nrow(lda_expose_docs(LDA_test)), 1814)
})

