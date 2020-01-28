context("tm_stem2readable")
library(tmutilsr)

test_that("default parameters", {
    expect_equal(nrow(tm_stem2readable(corpus_test)), 145073)
})
