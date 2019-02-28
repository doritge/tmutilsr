
library(readr)
library(tmutilsr)


# a test LDA object with 10 topics, 1814 documents & 8008 terms
LDA_test <- read_rds("LDA_test.rds")


doc_table <- lda_expose_docs(LDA_test)

usethis::use_data(LDA_test)

data("LDA_test")

