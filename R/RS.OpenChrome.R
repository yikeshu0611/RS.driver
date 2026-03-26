#' Open Chrome
#'
#' @param port port, defaulted is 4444
#'
#' @return a web
#' @export
#'
#' @examples RS.OpenChrome()
RS.OpenChrome <- function(port=4444){
  .remDr <<-　remoteDriver(
    browserName = "chrome",
    remoteServerAddr = "localhost",
    port = port
  )
  .remDr$open()
}
