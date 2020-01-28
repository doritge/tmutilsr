context("tm_make_corpus")
library(tmutilsr)

test_that("default parameters", {
    expect_equal(nrow(tm_make_corpus(docs_test)), 145073)
    expect_equal(ncol(tm_make_corpus(docs_test)), 3)
})

test_that("stem2readable parameter", {
    expect_equal(nrow(tm_make_corpus(docs_test, stem2readable = FALSE)), 145073)
})

test_that("custom_SW parameter", {
    expect_equal(nrow(tm_make_corpus(docs_test, custom_sw = my_stopwords)), 136949)
})
