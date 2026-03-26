#' 启动的命令
#'
#' @return cmd
#' @export
#'
rs.cmd <- function(){
    cmd1 <- paste0('cd /Users/zhangjing/Desktop/script/rselenium')
    cmd2 <- sprintf('java -Dwebdriver.chrome.driver="%s" -jar %s',
            ifelse(do::is.windows(),"chromedriver.exe","chromedriver"),
            list.files(system.file("data", package="RS.Driver"),'standalone'))
    clipr::write_clip(c(cmd1,cmd2))
}

