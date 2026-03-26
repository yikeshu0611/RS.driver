#' Open Chrome
#'
#' @param port port, defaulted is 4444
#'
#' @return a web
#' @export
#'
#' @examples RS.OpenChrome()
RS.OpenFirefox <- function(port=4445){
    .remDr <<-　remoteDriver(
        browserName = "firefox",
        remoteServerAddr = "localhost",
        port = port
    )
    .remDr$open()
}
