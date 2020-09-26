Sys.setenv(JAVA_HOME="C:\\Program Files\\Java\\jre1.8.0_231")
writeLines('PATH="${RTOOLS40_HOME}\\usr\\bin;${PATH}"', con = "~/.Renviron")
Sys.which("make")
install.packages("jsonlite",type="source")



library(wordcloud)
install.packages("wordcloud2")
library(wordcloud2)
library(KoNLP)
install.packages("KoNLP")
library(KoNLP)
install.packages("https://cran.r-project.org/src/contrib/Archive/KoNLP_0.80.2.tar.gz", repos = NULL, type="source",INSTALL_opts = c('--no-lock'))
library(KoNLP)
install.packages("rJava")
library(rJava)
library(RCurl)
install.packages("XML")
library(RCurl)
library(XML)
library(tm)
install.packages("tm")
library(NLP)
library(SnowballC)
install.packages("SnowballC")
library(RCurl)
t = readLines('D:|R|SteveJobs.txt')
install.packages("tau")
install.packages("RSQLite")
install.packages("devtools")
install.packages("stringr")

install.packages("digest")
install.packages("rlang")
install.packages("htmltools")
install.packages("jsonlite")
install.packages("yaml")
install.packages("htmlwidgets")
install.packages("base64enc")
install.packages("hash")
install.packages("Sejong")

library(RColorBrewer)
useSejongDic()
useSejongDic()
useNIADic()
extractNoun(noun)


setwd("D://R")
text <- readLines("SteveJobs.txt", encoding="UTF-8")


head(text)
library(KoNLP)
extractNoun(text[1])
buildDictionary(ext_dic="woorimalsam")
pal2 <- brewer.pal(8,"Dark2")
noun <- sapply(text,extractNoun, USE.NAMES = F)
noun
warnings()

noun2 <- unlist(noun)
wordcount <- table(noun2)
temp <- sort(wordcount, decreasing=T)[1:10]
temp
temp <- temp[-1]
barplot(temp,
        names.arg = names(temp),
        col="coral",
        main="빈도수 높은 단어",ylab="단어 빈도수")
wordclound(names(wordcount),
           freq=wordcount,
           scale=c(6,0,7),
           min.freq=3,
           reandom.order=F,
           rot.per=.1,
           colors=pal2)


s='너에게 묻는다 연탄재 함부로 발로 차지 마라 너는 누구에게 한번이라도 뜨거운 사람이었느냐 반쯤 깨진 연탄 언젠가는 나도 활활 타오르고 싶을 것이다 나를 끝 닿는데 까지 한번 밀어붙여 보고 싶은 것이다 타고 왔던 트럭에 실려 다시 돌아가면 연탄, 처음으로 붙여진 나의 이름도 으깨어져 나의 존재도 까마득히 뭉개질 터이니 죽어도 여기서 찬란한 끝장을 한번 보고 싶은 것이다 나를 기다리고 있는 뜨거운 밑불위에 지금은 인정머리없는 차가운, 갈라진 내 몸을 얹고 아랫쪽부터 불이 건너와 옮겨 붙기를 시간의 바통을 내가 넘겨 받는 순간이 오기를 그리하여 서서히 온몸이 벌겋게 달아 오르기를 나도 느껴보고 싶은 것이다 나도 보고 싶은 것이다 모두들 잠든 깊은 밤에 눈에 빨갛게 불을 켜고 구들장 속이 얼마나 침침하니 손을 뻗어 보고 싶은 것이다 나로 하여 푸근한 잠 자는 처녀의 등허리를 밤새도록 슬금슬금 만져도 보고 싶은 것이다'
extractNoun(s)


library(RCurl)
install.packages("RCurl")
library(XML)
t=readLines('http://en.wikipedia.org/wiki/Data_science')
d=htmlParse(t, asText = TRUE)
clean_doc = xpathSApply(d, "//p",xmlValue)
doc=Corpus(VectorSource(clean_doc))
inspect(doc)
doc = tm_map(doc, content_transformer(tolower))
doc = tm_map(doc, removeNumbers)
doc = tm_map(doc, removeWords, stopwords('english'))
doc = tm_map(doc, removePunctuation)
doc = tm_map(doc, stripWhitespace)
dtm= DocumentTermMatrix(doc)
dim(dtm)
inspect(dtm)
library(wordcloud)
m=as.matrix(dtm)
v=sort(colSums(m),decreasing = TRUE)
d=data.frame(word=names(v), freq = v)
View(d)
wordcloud(words = d$word, freq = d$freq, min.freq = 1, max.words = 100, random.order = FALSE, rot.per = 0.35)





Sys.setenv(JAVA_HOME="C:\\Program Files\\Java\\jre1.8.0_231")
library(wordcloud)
library(KoNLP)
library(RColorBrewer)
setwd("D://R")
text <- readLines("SteveJobs.txt", encoding="UTF-8")
buildDictionary(ext_dic="woorimalsam")
pal2 <- brewer.pal(8,"Dark2")
noun <- sapply(text,extractNoun, USE.NAMES = F)
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


Sys.setenv(JAVA_HOME="C:\\Program Files\\Java\\jre1.8.0_231")
library(wordcloud)
library(KoNLP)
library(RColorBrewer)
setwd("D://R")
text <- readLines("Obama.txt", encoding="UTF-8")
buildDictionary(ext_dic="woorimalsam")
pal2 <- brewer.pal(8,"Dark2")
noun <- sapply(text,extractNoun, USE.NAMES = F)
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

install.packages('readxl')
library(readxl)

getwd()
setwd("D://R")
text2 <- read.csv("도로교통공단_시도_시군구별_월별_교통사고(2016).csv", header=T)
head(text2)
agg <- aggregate(text2[,c(4,5,6)],by=list(text2$월),
                 FUN=sum)
agg
agg2 <- aggregate(text2[,c(4,5,6)],by=list(text2$시도),
                 FUN=sum)
agg2

str(text2)


library(ggplot2)
ggplot(text2,aes(x=월,y=사망자수)) +
  geom_bar(stat = "identity",
           width=0.5,
           fill="coral")

library(ggplot2)
ggplot(data=text2,aes(x=시도,y=발생건수)) +
  geom_line()

head(Orange)
ggplot(Orange,aes(x=age, y=circumference)) +
  geom_line()


setwd("D://R")
text3 <- read.csv("한국관광공사_전국 음식점 정보_20160906.csv", header=T)
head(text3)
chaesik <- subset(text3,카테고리3=="채식전문점")
chaesik[,c(1,6)]
