#' tmutilsr: Text mining utilities
#'
#' tmutilsr provides text processing utilities and extends basic R text mining packages,
#' such as topicmodels and stm, with commonly used processing functions
#'
#' \itemize{
#' \item `lda_trim_beta` - trims LDA beta table by terms and/or topics
#' \item `lda_table_topics` - spreads the term distribution over topics by topic
#' \item `lda_list_docs` - lists documents by their lead topic
#' \item `scrape_url` - scrape text from URL
#' \item `tm_stem2readable` - convert stemmed words to a readable format
#' \item `tm_make_corpus` - Create a clean, stemmed and tidy corpus
#' }
#'
#' @import tidyverse
#' @import tidytext
#' @keywords internal
"_PACKAGE"
