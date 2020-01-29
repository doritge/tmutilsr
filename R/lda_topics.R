#' Trim LDA beta table by terms and/or topics
#'
#' @description A topic modeling utility. Trims the beta table of the LDA
#'   object resulting from `topicmodels::LDA()` by the requested number of
#'   highest ranking terms and / or topics
#'
#' @param lda LDA object
#' @param n_terms Number of highest ranking terms per topic to display. Default 20
#' @param topics A list of topic numbers to return
#'
#' @return a tibble of highest ranking terms per topic and their beta values
#'
#' @import tidytext
#' @import topicmodels
#'
#' @export
lda_trim_beta <- function(lda, n_terms = 20, topics = NULL){
    # Trim beta table by number of terms and number of topics

    topic_terms <- lda %>%
        tidy() %>%
        arrange(topic, -beta) %>%
        group_by(topic) %>%
        filter(row_number() <= n_terms) %>%
        ungroup()

    if(!is.null(topics))
        topic_terms <- topic_terms %>%
            filter(topic %in% topics)

    return(topic_terms)
}

#' Spread the term distribution over topics by topic columns
#'
#' @description A topic modeling utility. Spreads the beta table of the LDA
#'   object resulting from `topicmodels::LDA()` into the higest ranking terms
#'   and their beta values in columns per topic. The number of requested terms
#'   and topics can be specified as well as whether to display the beta's
#'
#' @param lda LDA object
#' @param n_terms Number of highest ranking terms per topic to display
#' @param topics A list of topic numbers to display. Optional butfFor stm objects
#'   must be provided
#' @param beta return beta values? default yes
#'
#' @return a table that spreads for each topic a column of terms and their
#'   associated beta
#'
#' @export
lda_table_topics <- function(lda, n_terms = 20, topics = NULL, beta = TRUE){
    # Look at the topics from the term perspective

    if(beta)
        cols <- c("term", "beta")
    else
        cols <- c("term")

    if(is.null(topics)){
        if(class(lda)[1] == "LDA_Gibbs")
            topic_nums <- 1:attr(lda, "k")
        else
            if(class(lda)[1] == "STM")
                topic_nums <- 1:lda$settings$dim$K
    }else
        topic_nums <- topics

    topics_table <- lda %>%
        lda_trim_beta(n_terms, topics) %>%
        split(.$topic) %>%
        map(`[`, cols) %>%
        reduce(cbind)

    if(beta)
        colnames(topics_table) <- str_c(cols, sort(rep(topic_nums,2)))
    else
        colnames(topics_table) <- str_c(cols, topic_nums)

    return(topics_table)
}
