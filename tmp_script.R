
library(tidyverse)
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
aa <- lda_list_docs(LDA_test, n_ranks = 2, n_docs = 2, topics = c(1,5,10))

nrow(lda_list_docs(LDA_test, topics = c(1,5,10)))

ncol(lda_list_docs(LDA_test, n_ranks = 2))
####
documents <- read_csv("../ICVRplus proj/output/icvrp_filtered_docs.csv",
                          col_types = cols(document = col_character())) %>%
    filter(row_number() <= 1000)

my_stopwords <- read_csv("../TM examples/data/aux_data/project_stopwords_stm.csv")

tm_make_corpus <- function(documents, custom_sw = NULL, stem2readable = TRUE){
    corpus <- documents %>%
        select(document, text) %>%
        unnest_tokens(word, text) %>%
        anti_join(stop_words)

    if(!is.null(custom_sw))
        corpus <- anti_join(corpus, custom_sw)

    corpus <- corpus %>%
        filter(str_detect(word, "\\d", negate = TRUE)) %>% # remove words with numbers
        mutate(word = str_replace(word, "[^[:alnum:]]", "")) %>% # remove other non-alphanumeric characters
        mutate(stem = SnowballC::wordStem(word))

    if(stem2readable)
        corpus <- tm_stem2readable(corpus)

    corpus
}

aa <- tm_make_corpus(documents, stem2readable = FALSE)
cc <- tm_stem2readable(aa)

bb <- tm_make_corpus(documents, custom_sw = my_stopwords)
tm_stem2readable <- function(corpus){
    # mapp stemmed word to a readable word (by taking most frequent word)
    word_stem <- corpus %>%
        select(word, stem)

    mapping <- corpus %>%
        count(word, sort = TRUE) %>%
        left_join(word_stem) %>%
        group_by(stem) %>%
        arrange(-n) %>%
        filter(row_number() == 1) %>%
        ungroup()

    stem2read <- mapping$word
    names(stem2read) <- mapping$stem

    corpus <- corpus %>%
        mutate(stem = stem2read[stem])
}
bb <- tm_stem2readable(aa)

importFrom(magrittr,"%>%")
importFrom(purrr,map)
importFrom(purrr,reduce)
importFrom(rlang,"%@%")
importFrom(stringr,str_c)

#' @importFrom rlang "%@%"
