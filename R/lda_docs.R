#' List documents by their lead topic
#'
#' A topic modeling utility. List documents by their most representative topic
#' based on the gamma table of the LDA_ object resulting from
#' `topicmodels::LDA()`. Optionally, lower ranking topics per document are also
#' displayed
#'
#' @param lda LDA_ object
#' @param n_ranks Number of topic ranks to display (<=k)
#' @param n_docs Number of returned documents per highest ranking topic. Default
#'   all
#' @param topics A list of topic numbers to display
#'
#' @return A list of n_docs documents per main topic, each associated with
#'   n_ranks topic and their gamma
#'
#' @import tidytext
#' @import topicmodels
#'
#' @importFrom magrittr %>%
#' @importFrom rlang %@%
#'
#' @export
lda_list_docs <- function(lda, n_ranks = 1, n_docs = 0, topics = NULL){
    # List documents by lead topic

    k <- lda %@% k
    docs_table <- tibble(document = lda %@% documents)

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

    if(is.null(topics))
        return(docs_table)
    else
        return(filter(docs_table, rank1 %in% topics))
}
