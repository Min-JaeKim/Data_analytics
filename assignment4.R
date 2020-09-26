
st <- data.frame(state.x77)
str(st)
st[order(st$Population),]
st[order(st$Income,decreasing=T),]
st3 <- st[order(st$Illiteracy),]
head(st3,10)
mt.gear <- split(mtcars,mtcars$gear)
mt.gear$`4`

mt.gear.3 <- mt.gear$`3`
mt.gear.5 <- mt.gear$`5`
mt.gear.35 <- rbind(mt.gear.3,mt.gear.5)
mt.gear.35


mt.wt <- subset(mtcars, wt>=1.5 & wt<=3.0)
mt.wt

library(mlbench)
data("Ionosphere")
myds <- Ionosphere
str(iris)
myds 
myds.classv1 <- cbind(myds$V1,myds$Class)
myds.classv1
agg2 <- aggregate(myds[,-31], by=list(Ç°=myds$V1),
                  FUN=sd)
agg <- aggregate(myds[,10],by=list(Ç°Á¾=myds.classv1),
                 FUN=sd)
agg
agg2
head(iris)

myds <- Ionosphere
agg <- aggregate(myds[,3:10], by=list(myds$V1,myds$Class),
                 FUN=sd)
agg


idx4 <- sample(1:nrow(state.x77),size=20,
               replace=FALSE)
st20 <- state.x77[idx4,]
dim(st20)
head(st20)
st20
st.other <- state.x77[-st20,]
st.other



iris$Species
idx <- sample(1:nrow(iris),size=10,,replace=FALSE)
setosa <- subset(iris,iris$Species=="setosa")
setosa
versicolor <- subset(iris,iris$Species=="versicolor")
versicolor
virginica<- subset(iris,iris$Species=="virginica")
virginica
str(setosa)

idx
idx1 <- sample(1:nrow(setosa),size=10,,replace=FALSE)
iris.10.1 <- setosa[idx1,]
dim(iris.10)
head(iris.10)
iris.10.1
idx2 <- sample(1:nrow(versicolor),size=10,,replace=FALSE)
iris.10.2 <- versicolor[idx2,]
iris.10.2
idx3 <- sample(1:nrow(virginica),size=10,,replace=FALSE)
iris.10.3 <- virginica[idx3,]
iris.10.3
iris.10 <- rbind(iris.10.1,iris.10.2,iris.10.3)
iris.10










st <- data.frame(state.x77)
st1 <- st["Alabama","Area"]
st2 <- st["California","Area"]
st3 <- subset(st,Area>st1 & Area<st2)
st3[,c("Population","Income","Area")]






authors<-data.frame(
  surname=c("Twein","Venables","Tierney","Ripley","McNeil"),
  nationality=c("US","Australia","US","UK","Australia"),
  retired=c("yes",rep("no",4)))
authors
books<-data.frame(
  name=c("Johns","Venables","Tierney","Ripley","Ripley","McNeil"),
  title=c("ExploratoryDataAnalysis","ModernAppliedStatistics...","LISP-STAT","SpatialStatistics","StochasticSimulation","InteractiveDataAnalysis"),
  other.author=c(NA,"Ripley",NA,NA,NA,NA))


memory.size()
memory.limit()
x=rep(0,300000000)
memory.size()
y=rep(0,300000000)
memory.limit(10000)
memory.limit()
y=rep(0,300000000)



setwd("D:/R")
air <- read.csv("subway.csv",header=T)
head(air)
air2 <- read.csv("subway_latlong.csv",header=T)
head(air2)
subway.tot <- merge(air,air2,by.x=c("station"),by.y=c("STATION_CD"))
head(subway.tot)
agg <- aggregate(subway.tot[,c("on_tot","off_tot")],
                 by=list(subway.tot$station,subway.tot$stat_name),
                 FUN=sum)
agg

subway.tot2 <- subway.tot[grep("^2011",subway.tot$income_date),]
agg2 <- aggregate(subway.tot2[,c("on_tot","off_tot")],
                  by=list(subway.tot2$stat_name),
                  FUN=sum)
agg2
head(subway.tot2)
subway.tot2

subway.tot2 <- subway.tot[grep("^2011",subway.tot$income_date),]
agg3 <- aggregate(subway.tot2[,c("on_tot","off_tot")],
                  by=list(subway.tot2$LINE_NUM),
                  FUN=sum)
agg3







