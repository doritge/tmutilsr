context("scrape_text")
library(tmutilsr)

test_that("text retrieved", {
    expect_true(is.na(scrape_text("https://www.nytimes.com/2018/02/10/watching/homeland-season-7-premiere-catchup-what-you-need-to-know.html", ".evys1bk0")))
    expect_false(is.na(scrape_text("https://www.nytimes.com/2018/02/10/watching/homeland-season-7-premiere-catchup-what-you-need-to-know.html", "p")))

})
