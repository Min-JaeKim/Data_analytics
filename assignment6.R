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
        main="�󵵼� ���� �ܾ�",ylab="�ܾ� �󵵼�")
wordclound(names(wordcount),
           freq=wordcount,
           scale=c(6,0,7),
           min.freq=3,
           reandom.order=F,
           rot.per=.1,
           colors=pal2)


s='�ʿ��� ���´� ��ź�� �Ժη� �߷� ���� ���� �ʴ� �������� �ѹ��̶� �߰ſ� ����̾����� ���� ���� ��ź �������� ���� ȰȰ Ÿ������ ���� ���̴� ���� �� ��µ� ���� �ѹ� �о�ٿ� ���� ���� ���̴� Ÿ�� �Դ� Ʈ���� �Ƿ� �ٽ� ���ư��� ��ź, ó������ �ٿ��� ���� �̸��� �������� ���� ���絵 ����� ������ ���̴� �׾ ���⼭ ������ ������ �ѹ� ���� ���� ���̴� ���� ��ٸ��� �ִ� �߰ſ� �غ����� ������ �����Ӹ����� ������, ������ �� ���� ��� �Ʒ��ʺ��� ���� �ǳʿ� �Ű� �ٱ⸦ �ð��� ������ ���� �Ѱ� �޴� ������ ���⸦ �׸��Ͽ� ������ �¸��� ���Ӱ� �޾� �����⸦ ���� �������� ���� ���̴� ���� ���� ���� ���̴� ��ε� ��� ���� �㿡 ���� ������ ���� �Ѱ� ������ ���� �󸶳� ħħ�ϴ� ���� ���� ���� ���� ���̴� ���� �Ͽ� Ǫ���� �� �ڴ� ó���� ���㸮�� ������� ���ݽ��� ������ ���� ���� ���̴�'
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
text2 <- read.csv("���α������_�õ�_�ñ�����_����_������(2016).csv", header=T)
head(text2)
agg <- aggregate(text2[,c(4,5,6)],by=list(text2$��),
                 FUN=sum)
agg
agg2 <- aggregate(text2[,c(4,5,6)],by=list(text2$�õ�),
                 FUN=sum)
agg2

str(text2)


library(ggplot2)
ggplot(text2,aes(x=��,y=����ڼ�)) +
  geom_bar(stat = "identity",
           width=0.5,
           fill="coral")

library(ggplot2)
ggplot(data=text2,aes(x=�õ�,y=�߻��Ǽ�)) +
  geom_line()

head(Orange)
ggplot(Orange,aes(x=age, y=circumference)) +
  geom_line()


setwd("D://R")
text3 <- read.csv("�ѱ���������_���� ������ ����_20160906.csv", header=T)
head(text3)
chaesik <- subset(text3,ī�װ���3=="ä��������")
chaesik[,c(1,6)]