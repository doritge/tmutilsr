context("lda_table_topics")
library(tmutilsr)

test_that("default parameters", {
    expect_equal(nrow(lda_table_topics(LDA_test)), 20)
    expect_equal(ncol(lda_table_topics(LDA_test)), 20)
})

test_that("n_terms parameter", {
    expect_equal(nrow(lda_table_topics(LDA_test, n_terms = 10)), 10)
})

test_that("topics parameter", {
    expect_equal(ncol(lda_table_topics(LDA_test, topics = c(2,5,10,7))), 8)
})

test_that("beta parameter", {
    expect_equal(ncol(lda_table_topics(LDA_test, beta = FALSE)), 10)
    expect_equal(ncol(lda_table_topics(LDA_test, topics = c(2,5,10,7), beta = FALSE)), 4)
})
