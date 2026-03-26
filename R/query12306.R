input.from <- function(x,msg=F){
    RS.clear_text('//input[@id="fromStationText"]')
    RS.input_Text('//input[@id="fromStationText"]',x);Sys.sleep(0.5)

    txt <- RS.get_elements('//div[@id="top_cities"]') %>% html_text() %>% paste0(collapse = ',')
    if (grepl('无法匹配',txt)) stop(x,'输入不正确')
    (cities <- RS.get_elements('//div[@id="panel_cities"]/div/span') %>% set::grep_not_and('float:right') %>% html_text() %>% unique())
    ck <- cities == x
    if (!any(ck))  stop(x,'输入不正确')
    RS.click(sprintf('//div[@id="citem_%s"]',which(ck)-1))
    if (msg) message('出发站：',cities[ck])
}
input.to <- function(x,msg=F){
    RS.clear_text('//input[@id="toStationText"]')
    RS.input_Text('//input[@id="toStationText"]',x);Sys.sleep(0.5)

    txt <- RS.get_elements('//div[@id="top_cities"]') %>% html_text() %>% paste0(collapse = ',')
    if (grepl('无法匹配',txt)) stop(x,'输入不正确')
    (cities <- RS.get_elements('//div[@id="panel_cities"]/div/span') %>% set::grep_not_and('float:right') %>% html_text() %>% unique())
    ck <- cities == x
    if (!any(ck))  stop(x,'输入不正确')
    RS.click(sprintf('//div[@id="citem_%s"]',which(ck)-1))
    if (msg) message('到达站：',cities[ck])

    RS.click('//a[@id="query_ticket"]')
    Sys.sleep(0.5)
}


#' 查询火车票
#'
#'
#' @returns train
#' @export
#'
query.train <- function(from,to,date=NULL){
    suppressPackageStartupMessages(library(do,warn.conflicts = F,quietly = T))
    suppressPackageStartupMessages(library(rvest,warn.conflicts = F,quietly = T))
    if (is.null(date)) date <- as.character(Sys.Date())

    # 获取当前界面的情况
    url0 <- "https://kyfw.12306.cn/otn/leftTicket/init?linktypeid=dc&fs=上海,TIU&ts=北京,AOH&date=日期&flag=N,N,Y"
    url <- gsub('日期',date,url0)
    RS.open_Url(url)
    # 输入from和to
    input.from(from,T)
    input.to(to,T)


    (fromss <- RS.get_elements('//ul[@id="from_station_ul"]/li') %>% html_text())
    (toss <- RS.get_elements('//ul[@id="to_station_ul"]/li') %>% html_text())
    (checi <- RS.get_elements('//a[@title="点击查看停靠站信息"]') %>% html_text())
    message('共有',length(unique(checi)),'个车次')

    # 逐个点击，看看途径站
    (trs <- RS.get_elements('//div[@id="t-list"]/table/tbody/tr'))
    for (i in 1:length(trs)){
        if (i==1){
            media <- c()
            tbody <- RS.get_elements('//tbody[@id="queryLeftTable"]')
            checi.final <- NULL
        }
        clickit <- sprintf('tr[%s]//a[@title="点击查看停靠站信息"]',i)
        (alen <- length(html_elements(tbody,xpath = clickit)))
        if (alen>0){
            RS.click(sprintf('//tbody[@id="queryLeftTable"]/tr[%s]//div[@class="train"]/div/a',i))
            Sys.sleep(1)
            (si <- RS.get_elements(sprintf('//tbody[@id="queryLeftTable"]/tr[%s]//div[@class="station-bd"]',i)) %>%
                html_table() %>% do::list1() %>% as.data.frame() %>% do::select(j=2,drop=T))
            media <- unique(c(media,si[-c(1:max(which(si %in% fromss)))]))

            di <- data.frame(checi=html_elements(tbody,xpath = clickit) %>% html_text(),
                             终点站=paste0(toss[toss %in% si],collapse = ','))
            checi.final <- unique(rbind(checi.final,di))
        }
    }
    if (anyDuplicated(checi.final$checi)){
        checi.final <- do::dup.connect(checi.final,'checi','终点站')
        colnames(checi.final) <- do::Replace0(colnames(checi.final),' ')
        checi.final$Freq <- NULL
        checi.final$checi <- do::Replace0(checi.final$checi,' ')
        checi.final$终点站 <- do::Replace0(checi.final$终点站,' ')
        for (i in 1:nrow(checi.final)){
            checi.final$终点站[i] <- paste0(toss[toss %in% strsplit(checi.final$终点站[i],',')[[1]]],collapse = ',')
        }
    }

    message('共有',length(media),'个途径站')

    pb <- txtProgressBar(max = length(media),width = 25,style = 3)
    d1 <- lapply(media,function(mi){
        setTxtProgressBar(pb,which(media == mi))
        # 输入to
        input.to(mi)

        table <- RS.get_elements('//div[@id="t-list"]/table')

        head <- table %>% html_elements(xpath = 'thead/tr/th') %>% html_text() %>% do::Trim('\n') %>% do::Replace('\n','/')

        trs <- table %>% html_elements(xpath = 'tbody/tr')
        lapply(1:length(trs),function(i){
            td <- trs[i] %>% html_elements(xpath = 'td')
            if (length(td)==0) return()
            if (grepl('以下为同车车次变更接续方案',paste0(as.character(td),collapse = ','))) return()

            # 车次和类型
            (checi <- td[[1]] %>% html_elements(xpath = './/div[@class="train"]//a') %>% html_text())
            (station <- td[[1]] %>% html_elements(xpath = './/div[@class="cdz"]/strong') %>% html_text() %>% paste0(collapse = ','))
            (time <- td[[1]] %>% html_elements(xpath = './/div[@class="cds"]/strong') %>% html_text() %>% paste0(collapse = ','))
            (ls <- td[[1]] %>% html_elements(xpath = './/div[@class="ls"]/strong') %>% html_text() %>% paste0(collapse = ','))

            di <- data.frame(matrix(c(checi,station,time,ls,html_text(td)[-1]),nrow=1))
            colnames(di) <- head
            di
        }) %>% do.call(what=rbind)
    }) %>% do.call(what=plyr::rbind.fill)

    d1[d1 == '--'] <- ''
    d1[d1 == '候补'] <- ''
    d1[d1 == '无'] <- ''
    d1[d1 == '预订'] <- ''

    d1 <- d1[d1$车次 %in% checi,]
    if (nrow(d1)>0){
        for (i in colnames(d1)) if (all(nchar(d1[,i])==0)) d1[,i] <- NULL
        if (ncol(d1)>=5){
            d1 <- lapply(1:nrow(d1),function(i){
                ck <- nchar(as.character(d1[i,(which(colnames(d1) == '历时')+1):ncol(d1)])) == 0
                if (!all(ck)) d1[i,]
            }) %>% do.call(what=rbind)
            if (nrow(d1) >0){
                di <- do::col_split(d1$出发时间到达时间,',',colnames = c('出发时间','到达时间'))
                d1$出发时间到达时间 <- NULL
                d1 <- cbind(di,d1)

                di <- do::col_split(d1$`出发站/到达站`,',',colnames = c('出发站','途径站'))
                d1$`出发站/到达站` <- NULL
                d1 <- cbind(di,d1)
                d1 <- d1[order(d1$出发时间,d1$到达时间),]
                row.names(d1) <- NULL
                d1 <- dplyr::left_join(d1,checi.final,c('车次'='checi'))
                d1 <- d1[,unique(c('出发站','途径站','终点站',colnames(d1)))]
                colnames(d1)[colnames(d1) == '途径站'] <- '购票终点站'
            }else{
                d1 <- NULL
            }
        }else{
            d1 <- NULL
        }

    }else{
        d1 <- NULL
    }
    if (is.null(d1)){
        message('没有查到')
    }else{
        View(d1)
    }
}
