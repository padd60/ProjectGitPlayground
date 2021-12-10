install.packages("rvest")
library(rvest)
url<-"https://tv.naver.com/jtbc.youth"
html<-read_html(url,"UTF-8")

html_data<-html %>% html_nodes(".title a") %>% html_text() %>% data.frame()

#################################################
url<-"https://m.coupang.com/nm/search?q=아이패드"
html<-read_html(url,"UTF-8")

html_data2<-html %>% html_nodes(".info .title") %>% html_text() 
html_data3<-html %>% html_nodes(".info .discount-price strong") %>% html_text() 
data<-data.frame(title=html_data2,price=html_data3)


#####################################################
url<-"https://www.koreabaseball.com/Record/Player/HitterBasic/Basic1.aspx"
html<-read_html(url,"UTF-8")
data_name<-html %>% html_nodes(".tData01 tbody tr td:nth-child(2)") %>% html_text() 
data_avg<-html %>% html_nodes(".tData01 tbody tr td:nth-child(4)") %>% html_text() 
data2<-data.frame(name=data_name,avg=data_avg)
library(ggplot2)
library(dplyr)
order<- arrange(data2, avg)$name
ggplot(data = data2, aes(x =name, y = avg)) + geom_col() +coord_flip()+scale_x_discrete(limit=order)


####################################################
url<-"http://www.weather.go.kr/weather/observation/currentweather.jsp"
html<-read_html(url,"euc-kr")
data_loc<-html %>% html_nodes(".table_develop3 tbody tr td:nth-child(1)") %>% html_text() 
data_deg<-html %>% html_nodes(".table_develop3 tbody tr td:nth-child(6)") %>% html_text() 
data3<-data.frame(loc=data_loc,deg=data_deg)
data3$deg<-as.numeric(as.character(data3$deg))
data3<- data3 %>% arrange(desc(deg)) %>% head(20)
library(ggplot2)
library(dplyr)
order1<- arrange(data3, deg)$loc
ggplot(data = data3, aes(x =loc, y = deg)) + geom_col() +coord_flip()+scale_x_discrete(limit=order1)

###################################################

url<-"http://auto.danawa.com/auto/?Work=record"
html<-read_html(url,"UTF-8")
data_maker<-html %>% html_nodes(".clearFix:nth-child(3) .left .title a") %>% html_text() 
library(stringr)
data_maker<-str_replace_all(data_maker,"\\W","")
data_rate<-html %>% html_nodes(".clearFix:nth-child(3) .left .share") %>% html_text() 
data_rate<-str_replace_all(data_rate,"%","")
data4<-data.frame(maker=data_maker,rate=data_rate)
data4$rate<-as.numeric(as.character(data4$rate))
library(ggplot2)
library(dplyr)
order2<- arrange(data4,rate)$maker
ggplot(data = data4, aes(x =maker, y = rate)) + geom_col() +coord_flip()+scale_x_discrete(limit=order2)

#######################################################

url <- "https://finance.naver.com/sise/lastsearch2.naver"
html<-read_html(url, "euc-kr")
data_stockname<-html %>% html_nodes(".type_5  .tltle") %>% html_text()
data_stockcost<-html %>% html_nodes(".type_5 tbody tr td:nth-child(4)") %>% html_text()
data_stockname
