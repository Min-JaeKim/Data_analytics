# 02 DTM 구축 #
library(RCurl)
library(XML)

t = readLines('https://en.wikipedia.org/wiki/Data_science')
d = htmlParse(t, asText = TRUE)
clean_doc = xpathSApply(d,"//p", xmlValue)


library(tm)
library(SnowballC)

doc = Corpus(VectorSource(clean_doc))
inspect(doc)

doc = tm_map(doc, content_transformer(tolower))
doc = tm_map(doc, removeNumbers)
doc = tm_map(doc, removeWords, stopwords('english'))
doc = tm_map(doc, removePunctuation)
doc = tm_map(doc, stripWhitespace)


dtm = DocumentTermMatrix(doc)
dim(dtm)


inspect(dtm)


# 03 단어 구름 #
library(wordcloud)

m = as.matrix(dtm)
v = sort(colSums(m), decreasing = TRUE)
d = data.frame(word = names(v), freq = v)
wordcloud(words = d$word, freq = d$freq, min.freq = 1, max.words = 100, random.order = FALSE, rot.per = 0.35)

library(RColorBrewer)

pal = brewer.pal(11,"Spectral")
wordcloud(words = d$word, freq = d$freq, min.freq = 1, max.words = 50, random.order = FALSE, rot.per = 0.50, colors = pal)
wordcloud(words = d$word, freq = d$freq, min.freq = 1, max.words = 50, random.order = FALSE, rot.per = 0.50, colors = pal, family = "mono", font = 2)


library(wordcloud2)
wordcloud2(d)


d1 = d[1:200, ]        # 200개 단어만 표시
wordcloud2(d1, shape = 'diamond')
wordcloud2(d1, minRotation = pi/4, maxRotation = pi/4, rotateRatio = 1.0)


findFreqTerms(dtm, lowfreq = 12)

findAssocs(dtm, terms = 'field', corlimit = 0.7)

barplot(d[1:10, ]$freq, las = 2, names.arg = d[1:10,]$word, col = 'lightblue', main = '발생 빈도 상위 단어', ylab = '단어 빈도')

library(gapminder)
library(dplyr)

pop_siz = gapminder%>%filter(year==2007)%>%group_by(continent)%>%summarize(sum(as.numeric(pop)))
d = data.frame(word = pop_siz[, 1], freq = pop_siz[, 2])
wordcloud(words = d[, 1], freq = d[, 2], min.freq = 1, max.words = 100, random.order = FALSE, rot.per = 0.35)
wordcloud2(d)


# 05 영어 텍스트 마이닝을 이용한 한국어 처리 #

library(tm)
library(XML)
library(wordcloud2)
library(SnowballC)
library(RCurl)
t = readLines('https://ko.wikipedia.org/wiki/%EB%B9%85_%EB%8D%B0%EC%9D%B4%ED%84%B0')
d = htmlParse(t, asText = TRUE)
clean_doc = xpathSApply(d, "//p", xmlValue)


doc = Corpus(VectorSource(clean_doc))
inspect(doc)

doc = tm_map(doc, content_transformer(tolower))
doc = tm_map(doc, removeNumbers)
doc = tm_map(doc, removePunctuation)
doc = tm_map(doc, stripWhitespace)

dtm = DocumentTermMatrix(doc)
dim(dtm)
inspect(dtm)


m = as.matrix(dtm)
v = sort(colSums(m), decreasing = TRUE)
d = data.frame(word = names(v), freq = v)
d1 = d[1:500, ]                # 500개 단어만 표시
wordcloud2(d1)



# 06 KoNLP를 이용한 한국어 텍스트 마이닝 #

install.packages('KoNLP')
library(KoNLP)
useSystemDic()
useSejongDic()
useNIADic()


useSejongDic()
s='너에게 묻는다 연탄재 함부로 발로 차지 마라 너는 누구에게 한번이라도 뜨거운 사람이었느냐'
extractNoun(s)

SimplePos22(s)


t=readLines('https://ko.wikipedia.org/wiki/%EB%B9%85_%EB%8D%B0%EC%9D%B4%ED%84%B0')
d=htmlParse(t, asText=TRUE)
clean_doc=xpathSApply(d, "//p", xmlValue)


useSejongDic()

nouns = extractNoun(clean_doc)
mnous = unlist(nouns)
mnous_freq = table(mnous)
v = sort(mnous_freq, decreasing = TRUE)

wordcloud2(v)             # 모든 단어 표시 : [그림 11-14(a)]
v1 = v[1:100]              # 상위 100개 단어만 골라 표시 : [그림 11-14(b)]
wordcloud2(v1)
