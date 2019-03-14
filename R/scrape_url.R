#' Scrape text from URL
#'
#' @description Scrape text of a specific node tag in a given URL. All text
#'   segments are combine to one text object
#'
#' @param url the required URL
#' @param tag CSS node tag of required text
#'
#' @return A text object that combines all segments of the node tag
#'
#' @import rvest
#'
#' @importFrom magrittr %>%
#'
#' @export
scrape_text <- function(url, tag){
    nodes <- xml2::read_html(url) %>%
        html_nodes(tag)
    if(length(nodes) == 0) return(NA)
    html_text(nodes) %>%
        str_c(collapse = " ")
}
