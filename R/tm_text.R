#' Convert stemmed words to a readable format
#'
#' @description A text cleanup utility. It converts the words that have been
#'   stemmed to a readable format by selecting the most frequent word among those
#'   transformed to the same root.
#'
#' @param corpus object in a tidy format containing at least 'word' 'stem' columns
#'
#' @return the corpus where the 'stem' column is replaces with a readable word
#'
#' @export
tm_stem2readable <- function(corpus){
    # map stemmed word to a readable word (by taking most frequent word)
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

#' Create a clean, stemmed and tidy corpus
#'
#' @description A text cleanup utility. takes in a dataset of documents,
#'   cleans it, removes stopwords and performs Porter stemming using the SnowballC
#'   package. It outputs a tidy corpus with stemmed words. Optionally is takes a
#'   custom stopwords list and optionally transforms the stemmed words to a readable form
#'
#' @param documents object. Must contain a 'text' column
#' @param custom stopwords object. Must contain a 'word' column. Optional
#' @param stem to readable flag. defaults to TRUE
#'
#' @return a corpus that includes the original word and the stemmed word in the 'stem' column
#'
#' @export
tm_make_corpus <- function(documents, custom_sw = NULL, stem2readable = TRUE){
    # Create a corpus from a document dataset and clean it
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
