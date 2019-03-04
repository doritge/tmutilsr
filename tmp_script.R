
# library(readr)
library(tmutilsr)
library(tidytext)
library(magrittr)
# library(tidyr)
library(topicmodels)


# a test LDA object with 10 topics, 1814 documents & 8008 terms
LDA_test <- read_rds("LDA_filtered_all_10.rds")

tidy(LDA_test, matrix = "gamma")


doc_table <- lda_arrange_docs(LDA_test, n_ranks = 2, n_docs = 2)

usethis::use_data(LDA_test)

data("LDA_test")

lda_arrange_docs <- function(lda, n_docs = 0, n_ranks = 1){
    print(n_ranks)
    print(n_docs)
    table <- lda %>%
        tidytext::tidy(matrix = "gamma") %>%
        group_by(document) %>%
        arrange(-gamma) %>%
#        filter(dplyr::row_number() <= n_ranks) %>%
        ungroup()
    aa <- split(table, topic)

#    if (n_docs == 0) return(table)
}

    table <- table %>%
        group_by(topic) %>%
        arrange(-gamma) %>%
        filter(dplyr::row_number() <= n_docs) %>%
        ungroup() %>%
        arrange(topic, -gamma)
    return(table)
}
n_ranks<-2
table <- LDA_test %>%
    tidytext::tidy(matrix = "gamma") %>%
    group_by(document) %>%
    arrange(-gamma) %>%
    filter(dplyr::row_number() <= n_ranks) %>%
    ungroup()
a1<- table %>%
    group_by(document) %>%
    filter(dplyr::row_number() == 1) %>%
    rename(T1 = topic, gamma1 = gamma) %>%
    ungroup()

a2<- table %>%
    group_by(document) %>%
    filter(dplyr::row_number() == 2) %>%
    rename(T2 = topic, gamma2 = gamma) %>%
    ungroup()

aa<-dplyr::left_join(a1, a2)

aa <- tibble(document = LDA_test %@% documents) # (package rlang)
for (i in 1:n_ranks){
    tabgroup_by(document) %>%
        filter(dplyr::row_number() == 1) %>%
        rename(T1 = topic, gamma1 = gamma) %>%
        ungroup()
}
####

library(dplyr)
library(rlang)
library(readr)
library(tidytext)
library(topicmodels)

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

LDA_test <- read_rds("../VR rehabilitation/LDA_filtered_all_10.rds")
aa <- lda_arrange_docs(LDA_test, n_ranks = 2, n_docs = 2)

####
