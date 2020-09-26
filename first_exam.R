setwd("D:/R")
apple <- read.csv("apple.csv",header=T)
head(apple)
# ⑴ 상자그림 작성(품종별 무게 분포, 품종별 당도 분포, 품종별 산도 분포)
boxplot(weight~model,data=apple)
boxplot(sugar~model,data=apple)
boxplot(acid~model,data=apple)

# ⑵ apple 데이터의 80%를 훈련 데이터, 20%를 테스트 데이터로 분할하고 훈련 데이터를 이
# 용해 분류분석을 실시한 후 이를 의사결정 나무로 표시하시오.
library(rpart)
library(rpart.plot)

install.packages(rpart.plot)

installed.packages(rpart.plot)
apple.ds <- sample(1:nrow(apple), 0.8*nrow(apple))
apple.ts <- apple[-apple.ds,]
tree <- rpart(model ~ weight + sugar + acid + color, data=apple,
              subset = apple.ds, method="class")
par(mfrow=c(1,1),xpd=NA)
plot(tree)
text(tree,use.n = TRUE)

p=predict(tree,apple,type='class')
table(p,apple$model)
f = randomForest(model ~ weight + sugar + acid + color, data=apple)


std <- function(X){
  return((X-min(X)) / (max(X)-min(X)))
}
mydata <- apply(apple[,1:5],1,std)

head(apple)


library(cvTools)
k = 5
folds <- cvFolds(nrow(apple),K=k)
acc <- c()
for(i in 1:k){
  ts.idx <- folds$which == i
  apple.ds <- sample(1:nrow(apple), 0.8*nrow(apple))
  apple.ts <- apple[-apple.ds,]
  cl.tr <- factor(apple[-ts.idx,5])
  cl.ts <- factor(apple[ts.idx,5])
  pred <- knn(apple.ds,apple.ts,cl.tr,k=5)
  acc[i] <- mean(pred==cl.ts)
}
  



install.packages('rvest')
library(rvest)
library(XML)
library(plyr)

basic_url <- 'https://www.amazon.com/s?k=Korean+food&ref=nb_sb_noss_2'
read_html()
urls <- NULL
for(x in 0:5){
  urls[x+1] <- paste0(basic_url, x*15+1)
}
read_html(urls[1])
html_nodes(html, '.searchCont')

install.packages('selectr')
install.packages('xml2')
install.packages('jsonlite')
library(xml2)
library(rvest)
library(stringr)
url <- 'https://www.amazon.com/s?k=Korean+food&ref=nb_sb_noss_2'
title_html <- html_nodes('https://www.amazon.com/s?k=Korean+food&ref=nb_sb_noss_2','h1#title')
title <- html_text(title_html)
head(html)
color_html <- html_nodes('https://www.amazon.com/s?k=Korean+food&ref=nb_sb_noss_2','div # variation_color_name')
color_html <- html_nodes(color_html,'span.selection')
color <- html_text(color_html)
korean_food <- str_trim(color)
if(color=="Orange"){
  url <- 'https://www.amazon.com/s?k=Korean+food&ref=nb_sb_noss_2'
  title_html <- html_nodes('https://www.amazon.com/s?k=Korean+food&ref=nb_sb_noss_2','h1#title')
  title <- html_text(title_html)
}
korean_food <- title
print(korean_food)

url <- 'https://www.amazon.com/s?k=Korean+food&ref=nb_sb_noss_2'
library(XML)
doc <- htmlParse(url, encoding="utf-8")
prod_name <- xpathSApply(doc, "//ul[@id='a-page']//div[@class='name']",xmlValue)
titles <- html%>%html_nodes(".name")%>%html_text()



Sys.setenv(JAVA_HOME="C:\\Program Files\\Java\\jre1.8.0_231") 
library(wordcloud) 
library(KoNLP) 
library(RColorBrewer)
korean_food <- 'We will send you excellent products of korea directly from Korea. We will send the ordered product via EMS the next day, and we will always try to ship as soon as possible. We will do our best to satisfy our customers. Thank you! ● Eat easily the delicious Korean Ramen tteokbokki! ● Product type : Instant cooking food (can be stored at ambient temperature) ● Capacit y : 404g ● Manufacturer : Dongwon F&B (Korea) ● Ingredients : Tteokbokki(Korea), Gochujang(red pepper paste) sauce, etc. ● Recipe: 1. Macerate topokki rice cake in cold water for 3~4 minutes. 2. Add water (250ml), topokki rice cake, ramen and sauce, then boil them over low heat for 6~8 minutes. 3. You can also cook green onions, fish cakes, etc. as you like.'
buildDictionary(ext_dic="woorimalsam")
pal2 <- brewer.pal(8,"Dark2")
noun <- sapply(korean_food,extractNoun, USE.NAMES = F)
noun2 <- unlist(noun) 
wordcount <- table(noun2) 
temp <- sort(wordcount, decreasing = T)[1:10] 
temp 
temp <- temp[-1] 
wordcloud(names(wordcount),         
          freq=wordcount,          
          scale=c(6,0.7),          
          min.freq=3,          
          random.order = F,          
          rot.per = 1,         
          colors=pal2)