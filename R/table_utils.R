#' Expose document info by their lead topic
#'
#' For each document creates a table of documents associated with the highest
#' ranking topic and its gamma value. Optionally, The function extends the info
#' with the document metadata and lower ranking topics. Topic numbers can be
#' replaced with topic names
#'
#' @param lda An LDA object
#' @param n_docs Number of returned documents per topic. If 0 returns all
#'   documents
#' @param ranks Ranks of topics to return
#' @param topic_names A 2 column tibble - topic (key) and topic name
#' @param metadata A data frame of document metadata. Key column - document
#'
#' @return A table of n_docs documents per topic, each associated with the
#'   requested info
#'
#' @importFrom dplyr group_by ungroup arrange filter row_number
#' @importFrom magrittr %>%
#'
#' @export
lda_expose_docs <- function(lda, n_docs = 0, ranks = 1, topic_names = NULL, metadata = NULL){
    table <- lda %>%
        tidytext::tidy(matrix = "gamma") %>%
        group_by(document) %>%
        arrange(-gamma) %>%
        filter(dplyr::row_number() == ranks) %>%
        ungroup()
    if (!is.null(metadata)) table <- left_join(table, metadata)
    if (n_docs == 0) return(table)

    table %>%
        group_by(topic) %>%
        arrange(-gamma) %>%
        filter(row_number() <= n_docs) %>%
        ungroup() %>%
        arrange(topic, -gamma)
}
