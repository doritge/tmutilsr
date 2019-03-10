context("lda_list_docs")
library(tmutilsr)

test_that("default parameters", {
    expect_equal(nrow(lda_list_docs(LDA_test)), 1814)
})

test_that("n_ranks parameter", {
    expect_equal(ncol(lda_list_docs(LDA_test, n_ranks = 2)), 5)
    expect_equal(ncol(lda_list_docs(LDA_test, n_ranks = 11)), 21)
})

test_that("n_docs parameter", {
    expect_equal(nrow(lda_list_docs(LDA_test, n_docs = 2)), 20)
    expect_equal(nrow(lda_list_docs(LDA_test, n_docs = 1900)), 1814)
})

test_that("topics parameter", {
    expect_equal(nrow(lda_list_docs(LDA_test, n_docs = 2, topics = c(1, 5, 10))), 6)
    expect_equal(nrow(lda_list_docs(LDA_test, topics = c(1, 5, 10))), 542)
})

