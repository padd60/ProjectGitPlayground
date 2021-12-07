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
