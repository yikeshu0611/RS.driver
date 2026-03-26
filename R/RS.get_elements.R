#' RS.get_elements
#'
#' @param xpath xpaht
#'
#' @return RS.get_elements
#' @export
#'
RS.get_elements <- function(xpath){
    read_html(.remDr$getPageSource()[[1]][1]) %>% rvest::html_elements(xpath = xpath)
}
