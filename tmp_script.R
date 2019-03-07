
# library(readr)
library(tmutilsr)
library(tidytext)
library(magrittr)
# library(tidyr)
library(topicmodels)


####

library(dplyr)
library(purrr)
library(stringr)
#library(tidyr)
library(rlang)
library(readr)
library(tidytext)
library(topicmodels)

lda_trim_corpus <- function(lda, n_ranks = 1, n_docs = 0, topics = NULL){
    # Look at the topics from the document perspective

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
lda_plot_topics
#returns a list of term bar charts per topic


LDA_test <- read_rds("../VR rehabilitation/LDA_filtered_all_10.rds")
aa<-lda_trim_corpus(LDA_test, n_docs = 5, n_ranks = 2, topics = c(1,5,10))
aa <- lda_trim(LDA_test, n_terms = 10, topics = c(1,5,10))
aa <- lda_table_topics(LDA_test, n_terms = 10, beta = FALSE)



####
