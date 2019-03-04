#' Arrange the topic distribution per document
#'
#' Create a table were each row lists a document and its topic distribution
#' (gamma) in a descending order
#'
#' @param lda An LDA object
#' @param n_ranks Number of topic ranks (<=k)
#' @param n_docs Number of returned documents per highest ranking topic. By
#'   default reurns all documents
#'
#' @return A table of n_docs documents per main topic, each associated with
#'   n_ranks topic and its gamma
#'
#' @import dplyr
#' @import tidytext
#' @import topicmodels
#'
#' @importFrom magrittr %>%
#' @importFrom rlang %@%
#'
#' @export
lda_arrange_docs <- function(lda, n_ranks = 1, n_docs = 0){
    # Look at the topics from the document perspective

    k <- LDA_test %@% k
    docs_table <- tibble(document = LDA_test %@% documents)

    if(n_ranks > k) n_ranks <- k

    lda_table <- LDA_test %>%
        tidy(matrix = "gamma") %>%
        arrange(document, -gamma) %>%
        group_by(document)

    for (i in 1:n_ranks) {
        tmp <- filter(lda_table, row_number() == i) %>%
            rename_at(vars(topic, gamma), function(x) c(paste0("rank", i), paste0("gamma", i)))
        docs_table <- left_join(docs_table, tmp)
    }

    docs_table <- docs_table %>%
        arrange(rank1, -gamma1)

    if (n_docs != 0){
        docs_table <- docs_table %>%
            group_by(rank1) %>%
            filter(row_number() <= n_docs)
    }
    return(docs_table)
}
