10+20
a<-"hello world"
a

var1 <- c(1,2,3,4,5)
var1

var2<-c(1:100)
var2

var3<-seq(1,5)
var3

var4<-seq(1,10,by=2)
var4

str5<-c("hello", "world", "is","good")
str5
paste(str5, collapse=",")


x<-c(1,2,3)
mean(x)
max(x)
min(x)

y<-seq(1,100,by=2)
mean(y)
max(y)
min(y)

install.packages("ggplot2")

library(ggplot2)

x<-c("a","a","b","c")
qplot(x)

qplot(data=mpg,x=hwy)

qplot(data=mpg,x=cty)

qplot(data=mpg,x=drv, y=hwy)

qplot(data=mpg,x=drv, y=hwy, geom="boxplot")

english<-c(90,80,60,70)
english

math <- c(50,60,100,20)
math

df_midterm<- data.frame(english,math)
df_midterm

class<-c(1,1,2,2)
class

df_midterm<- data.frame(english,math, class)
df_midterm


df_midterm<- data.frame(english=c(90,20,38,47),math=c(87,58,98,57), class=c(1,1,2,2))
df_midterm

mean(df_midterm$math)

install.packages("readxl")
library(readxl)

df_midterm<-read_excel("excel_exam.xlsx")
df_midterm
mean(df_midterm$math)
mean(df_midterm$english)
mean(df_midterm$science)


df_exam_novar <- read_excel("excel_exam_novar.xlsx", col_names = F)
df_exam_novar


df_exam_sheet <- read_excel("excel_exam_sheet.xlsx", sheet = 3)
df_exam_sheet

df_csv_exam <- read.csv("csv_exam.csv")
df_csv_exam

write.csv(df_midterm, file="df_midterm.csv")

save(df_midterm, file="df_midterm.rda")
head(df_csv_exam,10)
tail(df_csv_exam)

View(df_csv_exam)
dim(df_csv_exam)
str(df_csv_exam)

summary(df_csv_exam)

mpg<-as.data.frame(ggplot2::mpg)
View(mpg)

head(mpg)
summary(mpg)

install.packages("dplyr")
library("dplyr")

df_raw<-data.frame(var1=c(1,2,3), var2=c(2,3,2))
df_new<-df_raw #df_raw를 복사해서 df_new 생성
df_new<-rename(df_new, v2=var2)


grade<-c(80,60,70,50,90)
grade

avg_grade<-mean(grade)
avg_grade

df_fruit<- data.frame(제품= c("사과","딸기","수박"), 가격=c(1800,1500,3000), 판매량=c(24,38,13))
df_fruit

mean(df_fruit$가격)
mean(df_fruit$판매량)

mpg<-as.data.frame(ggplot2::mpg)
mpg
mpg<-rename(mpg, city=cty)
mpg<-rename(mpg, highway=hwy)
head(mpg)

install.packages("foreign")
library(foreign)
library(dplyr)
library(ggplot2)
library(readxl)
raw_welfare <- read.spss(file = "Koweps_hpc10_2015_beta1.sav", to.data.frame = T)

welfare <- raw_welfare
welfare <- rename(welfare,
                  sex = h10_g3,
                  birth = h10_g4,
                  marriage = h10_g10,
                  religion = h10_g11,
                  income = p1002_8aq1,
                  code_job = h10_eco9,
                  code_region = h10_reg7)
table(welfare$sex)
welfare$sex <- ifelse(welfare$sex == 9, NA, welfare$sex)
table(is.na(welfare$sex))
welfare$sex <- ifelse(welfare$sex == 1, "male", "female") 
table(welfare$sex)
qplot(welfare$sex)
summary(welfare$income)
qplot(welfare$income)
qplot(welfare$income) + xlim(0, 1000)
welfare$income <- ifelse(welfare$income %in% c(0, 9999), NA, welfare$income)
table(is.na(welfare$income))
sex_income <- welfare %>% filter(!is.na(income)) %>% group_by(sex) %>%
  summarise(mean_income = mean(income))
sex_income
ggplot(data = sex_income, aes(x = sex, y = mean_income)) + geom_col()

class(welfare$birth)

summary(welfare$birth)

qplot(welfare$birth)

table(is.na(welfare$birth))

welfare$age <- 2021 - welfare$birth +1
summary(welfare$age)

qplot(welfare$age)

age_income <- welfare %>% 
  filter(!is.na(income)) %>% 
  group_by(age) %>% 
  summarise(mean_income=mean(income))

table(age_income)
head(age_income)

ggplot(data=age_income, aes(x=age, y=mean_income)) + geom_line()

welfare <- welfare %>%
  mutate(ageg = ifelse(age < 30, "young",
                       ifelse(age <= 59, "middle", "old")))

ageg_income <- welfare %>% 
                  filter(!is.na(income)) %>% 
                    group_by(ageg) %>% 
                      summarise(mean_income=mean(income))

head(ageg_income)

ggplot(data=ageg_income, aes(x=ageg, y=mean_income)) + geom_col() + scale_x_discrete(limits=c('young', 'middle', 'old'))

sex_income <- welfare %>% 
  filter(!is.na(income)) %>% 
  group_by(ageg, sex) %>% 
  summarise(mean_income=mean(income))

sex_income


ggplot(data=sex_income, aes(x=ageg,y=mean_income, fill=sex)) + geom_col(position = "dodge") + scale_x_discrete(limits=c('young', 'middle', 'old'))


sex_age<- welfare %>% 
            filter(!is.na(income)) %>% 
              group_by(age, sex) %>% 
                summarise(mean_income=mean(income))

ggplot(data=sex_age, aes(x=age, y=mean_income, col = sex)) + geom_line()

list_job<-read_excel("Koweps_Codebook.xlsx", col_names = T, sheet = 2)

head(list_job)

welfare<-left_join(welfare, list_job, id = "code_job")

welfare %>% 
  filter(!is.na(code_job)) %>% 
  select(code_job, job) %>% 
  head(10)

table(welfare$code_job, welfare$job)

job_income<-welfare %>% 
  filter(!is.na(job) & !is.na(income)) %>% 
  group_by(job) %>% 
  summarise(mean_income=mean(income))

head(job_income)

top10<-job_income %>% 
  arrange(desc(mean_income)) %>% 
  head(10)

top10

ggplot(data=top10, aes(x=reorder(job,mean_income), y=mean_income)) + geom_col() + coord_flip()


install.packages("extrafont") 
library(extrafont)
font_import()

theme_set(theme_grey(base_family='NanumGothic'))

ggplot(data=top10, aes(x=reorder(job,mean_income), y=mean_income)) + geom_col() + coord_flip()

bottom10<-job_income %>% 
  arrange((mean_income)) %>% 
  head(10)

bottom10

ggplot(data=bottom10, aes(x=reorder(job,-mean_income), y=mean_income)) + geom_col() + coord_flip() + ylim(0,150)

View(top10)
View(bottom10)

ggplot(data=top10, aes(x=reorder(job,mean_income), y=mean_income)) + geom_col() + coord_flip()


job_male<-welfare %>% 
  filter(!is.na(job) & sex == 'male') %>% 
  group_by(job) %>% 
  summarise(n = n()) %>% 
  arrange(desc(n)) %>% 
  head(10)

job_male

job_female<-welfare %>% 
  filter(!is.na(job) & sex == 'female') %>% 
  group_by(job) %>% 
  summarise(n = n()) %>% 
  arrange(desc(n)) %>% 
  head(10)

job_female

table(welfare$religion)

table(welfare$marriage)

welfare$religion <- ifelse(welfare$religion == 1, "yes", "no") 
table(welfare$religion)

welfare$group_marriage<-ifelse(welfare$marriage == 1, 'marriage',
                               ifelse(welfare$marriage == 3, 'divorce', NA))

table(welfare$group_marriage)
table(is.na(welfare$group_marriage))

religion_marriage<-welfare %>% 
  filter(!is.na(group_marriage)) %>% 
  group_by(religion, group_marriage) %>% 
  summarise(n=n()) %>% 
  mutate(tot_group = sum(n)) %>% 
  mutate(pct = round(n/tot_group*100, 1))

religion_marriage

divorce<-religion_marriage %>% 
  filter(group_marriage == 'divorce') %>% 
  select(religion, pct)

divorce

ggplot(data = divorce, aes(x = religion, y = pct)) + geom_col()

ageg_marriage <- welfare %>% 
  filter(!is.na(group_marriage)) %>% 
  group_by(ageg, group_marriage) %>% 
  summarise(n = n()) %>% 
  mutate(tot_group=sum(n)) %>% 
  mutate(pct=round(n/tot_group*100, 1))

ageg_marriage

ageg_divorce <- ageg_marriage %>% 
  filter(ageg != 'young' & group_marriage == 'divorce') %>% 
  select(ageg, pct)

ageg_divorce

ageg_religion_marriage <- welfare %>% 
  filter(!is.na(group_marriage) & ageg != 'young') %>% 
  group_by(ageg,religion,group_marriage) %>% 
  summarise(n=n()) %>% 
  mutate(tot_group=sum(n)) %>% 
  mutate(pct=round(n/tot_group*100, 1))

ageg_religion_marriage

df_divorce<-ageg_religion_marriage %>% 
  filter(group_marriage == 'divorce') %>% 
  select(ageg, religion, pct)

df_divorce

ggplot(data=df_divorce,aes(x=ageg, y=pct, fill = religion)) + geom_col(position = 'dodge')

list_region <- data_frame(code_region=c(1:7),
                          region=c('서울',
                                   '수도권(인천,경기)',
                                    '부산/경남/울산',
                                    '대구/경북',
                                    '대전/충남',
                                    '강원/충북',
                                    '광주/전남/전북/제주도'))

list_region

welfare<-left_join(welfare, list_region, id="code_region")

welfare %>% 
  select(code_region, region) %>% 
  head()

region_ageg<-welfare %>% 
  group_by(region, ageg) %>% 
  summarise(n=n()) %>% 
  mutate(tot_group = sum(n)) %>% 
  mutate(pct=round(n/tot_group*100,2))

head(region_ageg)

ggplot(data=region_ageg, aes(x=region, y=pct, fill = ageg)) + geom_col() + coord_flip()



list_order_old<-region_age %>% 
  filter(ageg=='old') %>% 
  arrange(pct)

list_order_old

order <- list_order_old$region
order

ggplot(data = region_ageg, aes(x = region, y = pct, fill = ageg)) + geom_col() +
  coord_flip() +
  scale_x_discrete(limits = order)

region_ageg$ageg <- factor(region_ageg$ageg,
                           level = c("old", "middle", "young"))

class(region_ageg$ageg)
levels(region_ageg$ageg)


install.packages("KoNLP", 
                 repos = c("https://forkonlp.r-universe.dev",
                           "https://cloud.r-project.org"),
                 INSTALL_opts = c("--no-multiarch")
)
install.packages("multilinguer")
multilinguer::install_jdk()
library("KoNLP")
install.packages("multilinguer")
library(multilinguer)
install.packages("multilinguer")

library(KoNLP)
multilinguer::install_jdk()

install.packages(c("hash", "tau", "Sejong", "RSQLite", "devtools", "bit", "rex", "lazyeval", "htmlwidgets", "crosstalk", "promises", "later", "sessioninfo", "xopen", "bit64", "blob", "DBI", "memoise", "plogr", "covr", "DT", "rcmdcheck", "rversions"), type = "binary")
install.packages("remotes")
remotes::install_github('haven-jeon/KoNLP', upgrade = "never", INSTALL_opts=c("--no-multiarch"))
remotes::install_gitlab("mrchypark/multilinguer")
install_jdk()
library("KoNLP")
library(KoNLP)
extractNoun('이 영화 정말 재미있다')
remotes::install_github("mrchypark/multilinguer")
install.packages("rJava")
library(KoNLP)
install.packages("KoNLP")

remotes::install_github('haven-jeon/KoNLP', upgrade = "never", INSTALL_opts=c("--no-multiarch"))
install.packages('devtools')
devtools::install_github('haven-jeon/KoNLP')
library(KoNLP)
library(rJava)
update.packages()
library(package)
install.packages("package")
install.packages("rlang")
remotes::install_github('haven-jeon/KoNLP', upgrade = "never", INSTALL_opts=c("--no-multiarch"))

install.packages('devtools')
devtools::install_github('haven-jeon/KoNLP')

Sys.setenv("JAVA_HOME"='/Library/Java/JavaVirtualMachines/jdk-17.0.1.jdk/Contents/Home')
dyn.load('/Library/Java/JavaVirtualMachines/jdk-11.0.12.jdk/Contents/Home/lib/server/libjvm.dylib')



#-------------------------------------------------------------
#여기부터 텍스트 마이닝 및 워드 클라우드 코드

library(KoNLP)
extractNoun('이 영화 정말 재미있다')
install.packages("memoise")
library(dplyr)
useNIADic()
txt <- readLines("hiphop.txt")
library(readxl)
head(txt)
install.packages("stringr")
library(stringr)
txt <- str_replace_all(txt, "\\W", " ")
extractNoun("대한민국 만세")
extractNoun("대한민국의 영토는 한반도와 그 부속도서로 한다")

nouns <- extractNoun(txt)
wordcount <- table(unlist(nouns))
df_word <- as.data.frame(wordcount, stringsAsFactors = F)
df_word <- rename(df_word,
                  word = Var1,
                  freq = Freq)
df_word <- filter(df_word, nchar(word) >= 2)
top_20 <- df_word %>% arrange(desc(freq)) %>% head(20)
top_20


install.packages("wordcloud")
library(wordcloud)
library(RColorBrewer)
pal <- brewer.pal(8,"Dark2")
set.seed(1234)
wordcloud(words = df_word$word,
          freq = df_word$freq,
          min.freq = 2,
          max.words = 200,
          random.order = F,
          rot.per = .1,
          family="AppleGothic",
          scale = c(4, 0.3), colors = pal)


pal <- brewer.pal(9,"Blues")[5:9]
set.seed(1234)
wordcloud(words = df_word$word,
          freq = df_word$freq,
          min.freq = 2,
          max.words = 200,
          random.order = F,
          rot.per = .1,
          family="AppleGothic",
          scale = c(4, 0.3), colors = pal)


twitter <- readLines("kim_president.txt")

twitter <- str_replace_all(twitter, "\\W", " ")
head(twitter)
nouns <- extractNoun(twitter)
wordcount <- table(unlist(nouns))
df_word <- as.data.frame(wordcount, stringsAsFactors = F)
df_word <- rename(df_word, word = Var1,
                  freq = Freq)
df_word <- filter(df_word, nchar(word) >= 2)
top20 <- df_word %>%
  arrange(desc(freq)) %>% head(20)
top20
library(ggplot2)
order <- arrange(top20, freq)$word
ggplot(data = top20, aes(x = word, y = freq)) +
  ylim(0, 2500) + geom_col() + coord_flip() +
  scale_x_discrete(limit = order) + 
  geom_text(aes(label = freq), hjust = -0.3)

pal <- brewer.pal(8,"Dark2")
set.seed(1234)
wordcloud(words = df_word$word,
          freq = df_word$freq,
          min.freq = 10,
          max.words = 200,
          random.order = F,
          rot.per = .1,
          family="AppleGothic",
          scale = c(6, 0.2), colors = pal)
#-------------------------------------------------------------
# 데이터시각화
install.packages("ggiraph")
install.packages("ggiraphExtra")
library(ggplot2)
library(ggiraph)
library(ggiraphExtra)
str(USArrests)
head(USArrests)
library(tibble)
crime <- rownames_to_column(USArrests, var = "state")
crime$state <- tolower(crime$state)
str(crime)
library(ggplot2)
states_map <- map_data("state") 
str(states_map)

install.packages("stringi")
devtools::install_github("cardiomoon/kormaps2014")

library(kormaps2014)
korpop1
ggChoropleth(data = korpop1, aes(fill = pop,
                                 map_id = code,
                                 tooltip = name),
             map = kormap1, interactive = T)
tbc

ggChoropleth(data = tbc, aes(fill = NewPts,
                             map_id = code,
                             tooltip = name),
             map = kormap1,
             interactive = T)

install.packages("plotly")
library(plotly)
library(ggplot2)
p <- ggplot(data = mpg, aes(x = displ, y = highway, col = drv)) + geom_point()
ggplotly(p)

p <- ggplot(data = diamonds, aes(x = cut, fill = clarity)) + geom_bar(position = "dodge")
ggplotly(p)

install.packages("dygraphs")
library(dygraphs)
economics <- ggplot2::economics
head(economics)
library(xts)
eco <- xts(economics$unemploy, order.by = economics$date)
head(eco)
dygraph(eco)
dygraph(eco) %>% dyRangeSelector()
eco_a <- xts(economics$psavert, order.by = economics$date)
eco_b <- xts(economics$unemploy/1000, order.by = economics$date)
eco2 <- cbind(eco_a, eco_b)
colnames(eco2) <- c("psavert",
                    "unemploy")
head(eco2) 
dygraph(eco2) %>% dyRangeSelector()
mpg <- as.data.frame(ggplot2::mpg)
library(dplyr)
mpg_diff <- mpg %>%
  select(class, cty) %>%
  filter(class %in% c("compact", "suv"))
head(mpg_diff)
table(mpg_diff$class)
t.test(data = mpg_diff, cty ~ class, var.equal = T)

mpg_diff2 <- mpg %>% select(fl, cty) %>%
  filter(fl %in% c("r", "p"))

table(mpg_diff2$fl)

t.test(data = mpg_diff2, cty ~ fl, var.equal = T)


head(mtcars)
car_cor <- cor(mtcars)
round(car_cor, 2)

install.packages("corrplot")
library(corrplot)
corrplot(car_cor)
corrplot(car_cor, method = "number")

col <- colorRampPalette(c("#BB4444", "#EE9988", "#FFFFFF", "#77AADD", "#4477AA"))

corrplot(car_cor,
         method = "color", 
         col = col(200), 
         type = "lower",
         order = "hclust", 
         addCoef.col = "black", 
         tl.col = "black", 
         tl.srt = 45,
         diag=F)


################################################

exam <- read.csv("csv_exam.csv")
exam
exam[]
exam[1,]
exam[1,1]
exam[1,'science']
exam[,'science']
exam[,c('class','math','science')]
exam[exam$math >= 50, "english"]
exam[exam$math >= 50, c("english", "science")]

exam$tot <- (exam$math + exam$english + exam$science)/3
aggregate(data=exam[exam$math >= 50 & exam$english >= 80,], tot~class, mean)

mpg <- ggplot2::mpg
x <- boxplot(mpg$cty)

x$stats[,1]

x

