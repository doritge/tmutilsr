context("lda_trim_beta")
library(tmutilsr)

test_that("default parameters", {
    expect_equal(nrow(lda_trim_beta(LDA_test)), 200)
})

test_that("n_terms parameter", {
    expect_equal(nrow(lda_trim_beta(LDA_test, n_terms = 40)), 400)
})

test_that("topics parameter", {
    expect_equal(nrow(lda_trim_beta(LDA_test, topics = c(3,7,9))), 60)
    expect_equal(nrow(lda_trim_beta(LDA_test, n_terms = 30, topics = c(3,7,9))), 90)
})
