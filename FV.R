require("rvest");require("data.table");require("pbapply")
# *************************************************
#                 HELPER FUNCTIONS
# *************************************************
toPct <- function(x){round(as.numeric(gsub("%","",x))/100,4)}
formatVOL <- function(x){as.numeric(gsub(",","",x))}
formatKMB <- function(x)
{
  tmp <- gsub("K","e3",x)
  tmp <- gsub("M","e6",tmp)
  tmp <- gsub("B","e9",tmp)
  as.numeric(tmp)
}
# *************************************************
#             SCRAPE FINVIZ TOP GAINERS
# *************************************************
url <- "https://finviz.com/screener.ashx?v=111&s=ta_topgainers"
data <- read_html(url)
PAGES <- data %>% html_nodes(".screener-pages") %>% length()
LAST <- 20*PAGES+1

accPAGES <- lapply(as.list(seq(1,LAST,20)), function(x) paste0("https://finviz.com/screener.ashx?v=111&s=ta_topgainers&r=",x,""))

getFinViz = function(x)
{
  Sys.sleep(10)
  data <- read_html(x)
  data %>% html_nodes("table") %>% .[[17]] %>% html_table(header=TRUE,fill=TRUE)
}


finViz1 <- pblapply(accPAGES,function(x){
  tmp <- try(getFinViz(x))
  if(!inherits(tmp,'try-error'))
    tmp
})

finViz1 <- finViz1[lapply(finViz1,length)>0]
dat <- as.data.frame(rbindlist(finViz1,use.names = TRUE),stringsAsFactors=FALSE)
dat$Date <- rep(Sys.Date(),nrow(dat))
dat$`Market Cap` <- formatKMB(dat$`Market Cap`)
dat$`P/E` <- as.numeric(dat$`P/E`)
dat$Price <- as.numeric(dat$Price)
dat$Change <- toPct(dat$Change)
dat$Volume <- formatVOL(dat$Volume)
dat <- dat[,2:ncol(dat)]

write.table(dat,"FINVIZtg.csv",sep=",")

















