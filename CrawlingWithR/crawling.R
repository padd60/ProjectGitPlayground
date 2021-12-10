# 웹 크롤링 연습

# 크롤링을 위한 package
install.packages("rvest")

library(rvest)
library(ggplot2)
library(dplyr)
library(stringr)

url <- "https://www.op.gg/champion/statistics"
html<-read_html(url,"UTF-8")

data_name <- html %>% 
  html_nodes(".champion-index-table .champion-trend-tier-MID tr td:nth-child(4) a div:nth-child(1)") %>% 
  html_text()

data_pic <- html %>% 
  html_nodes(".champion-index-table .champion-trend-tier-MID tr td:nth-child(6)") %>% 
  html_text()

lol <- data.frame(champion = data_name, picRatio = data_pic)

lol <- head(lol, 20)

class(lol$picRatio)

lol$picRatio <- str_sub(lol$picRatio,1,nchar(lol$picRatio)-1)

lol$picRatio <- as.numeric(lol$picRatio)

ggplot(data = lol, aes(x = champion, y = picRatio)) + geom_col() + coord_flip()
