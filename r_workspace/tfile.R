
# 웹 스크래핑
install.packages("RCurl")
install.packages("XML")
library(RCurl)
library(XML)
library(httr)
library(stringr)
url <- "https://www.coupang.com/np/search?q=%EC%97%AC%EC%84%B1%ED%81%AC%EB%A1%9C%EC%8A%A4%EB%B0%B1"
get0<- GET(url)
doc <- htmlParse(get0, encoding="UTF-8")

prod_name <- xpathSApply(doc, "//ul[@id='productList']//div[@class='name']", xmlValue) 
prod_name

# p.293
prod_name <- gsub('\n', '', prod_name)
prod_name <- gsub('\t', '', prod_name)
prod_name <- gsub(' ', '', prod_name)
prod_name

price     <- xpathSApply(doc, "//ul[@id='productList']//strong[@class='price-value']", xmlValue)
price

# p.294
df <- data.frame(상품명=prod_name, 가격=price) 
df

df$상품명 <- format(df$상품명, justify = "left")
df
