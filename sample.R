z <- matrix(1:20, nrow=4, ncol=5)
z
z2 <- matrix(1:20,nrow=4,ncol=5,byrow=T)
z2
x <- 1:4
y <- 5:8
z <- matrix(1:20,nrow=4,ncol=5)
m1 <- cbind(x,y)
m1
m2 <- rbind(x,y)
m2
m3 <- rbind(m2,x)
m3
m4 <- cbind(m1,y)
m4
m5 <- cbind(z,x)
m5
z[2,3]
z[1,4]
z[2,]
z[2,1:3]
sum(z[1,c(1,2,4)])
z[,c(1,4)]
city <- c("Seoul","Tokyo","Washington")
rank <- c(1,3,2)
city.info <- data.frame(city,rank)
city.info
absent <- matrix(c("JAN",10,
                   "FEB",8,
                   "MAR",14,
                   "APR",15,
                   "MAY",9,
                   "JUN",10,
                   "JUL",15,
                   "AUG",12,
                   "SEP",9,
                   "OCT",7,
                   "NOV",8,
                   "DEC",7),
                  nrow=2, ncol=12)
absent
absent <- c(10,8,14,15,9,10,15,12,9,7,8,7)
names(absent) <- c("JAN", "FEB", "MAR", "APR", "MAY", "JUN","JUL","AUG","SEP","OCT","NOV","DEC")
absent
sum(absent[,])
month <- c("JAN","FEB","MAR","APR","MAY","JUN","JUL",
           "AUG","SEP","OCT","NOV","DEC")
num <- c(10,8,14,15,9,10,15,12,9,7,8,7)
absent <- data.frame(month,num)
absent

mean(absent["JUL":"DEC"])


v1[v1 %% 2 == 0] <- v1[v1 %% 2 == 0]*2
v1


sum(absent[2,c(1:6)])
d1 <- 1:50
d2 <- 51:100
d1
d2
length(d2)
d1+d2
d2-d1
d1*d2
d2/d1
sum(d1)
sum(d1+d2)
max(d2)
min(d2)
mean(d2)-mean(d1)
sort(d1,decreasing=TRUE)
d3[1:10] <- d1[1:10]
d3[11:20] <- d2[1:10]
d3
mtcars
nrow(mtcars)
ncol(mtcars)
colnames(mtcars)
min(mtcars$mpg)
colnames(mt)
mtcars["Honda Civic",c("mpg","gear")]
mt2 <- subset(mtcars,subset(mt3,mpg>=colnames(Pontiac)))
mt3 <- colnames(mtcars)
mt2 <- subset(mtcars,max(mtcars$mpg))
mt4 <- subset(mtcars) %% max(mtcars$mpg)
rownames(mtcars[which.max(mtcars$mpg),])
rownames(st[which.max(st$Life.Exp),])

st4 <- subset(st,Income>="Pennsylvania")
st4

mt4 <- subset(mtcars,gear==4)
mt4

mt5 <- mtcars["Pontiac Firebird","mpg"]
mt5
mt6 <- subset(mtcars,mpg>mt5)
rownames(mt6)

mean(mtcars$mpg)

unique(mtcars[,"gear"])

class(airquality)
head(airquality)
air <- airquality
ai1 <- air[which.max(air$Temp),5:6]
ai1

ai2 <- subset(airquality,Month==6)
ai2
airquality[which.max(airquality$Wind),]


ai3 <- subset(airquality,Month==7)
ai3
mean(ai3$Temp)

ai4 <- subset(airquality,Month==5)
ai5 <- subset(ai4,Ozone!="NA")
mean(ai5$Ozone)

subset(airquality,Ozone>100)

head(swiss)

rownames(swiss[which.max(swiss$Agriculture),])


swiss[subset(swiss,Catholic>=80),2]
idx <- which(swiss$Catholic>=80)
swiss.big <- swiss[idx,2]
swiss.big

idx2 <- which(swiss$Examination<20&swiss$Agriculture<50)
swiss.big <- swiss[idx2,2:3]
swiss.big

swiss
swiss[,2]
swiss.m <- as.matrix(swiss[,2])
head(swiss.m)


subset(swiss,sort(swiss$Agriculture,decreasing=TRUE))


swiss[sort(swiss$Agriculture,decreasing=TRUE),]


order.swiss <- swiss[order(desc(swiss$Agriculture)),]
order.swiss


rownames(mt4[which.min(mt4$mpg),])

Means(mtcars$mpg)
st2 <- subset(st,Illiteracy>=2.0)
rownames(st2)
iris[,c(1:2)]
iris[1:2,c(1:2)]
iris[1:2,1:3]
dim(iris)
nrow(iris)
ncol(iris)
colomnames(iris)
colnames(iris)
head(iris)
tail(iris)
str(iris)
unique(iris[,5])
table(iris,"Species")
table(iris[,"Species"])
couSums(iris[,-5])
colSums(iris[,-5])
class(mtcars)
colMeans(iris[,-5])
colSums(iris[,c(1:3)])
t(z)
t(z)
z
IR.1 <- subset(iris,Species="setosa")
IR.1
IR.1 <- subset(iris,Species=="setosa")
IR.1
is.matrix(iris)
class(iris)
is.matrix(state.x77)
head(state.x77)


setwd("D:/R/data")
st1 <- subset(state.x77,Income>=5000)




st <- data.frame(state.x77)
st
colnames(st)
dim(st)
str(st)
rowSums(st)
rowMeans(st)

st <- data.frame(state.x77)
st2 <- subset(st,Income>=5000)
my.state <- st2[,c("Income","Population","Area")]
setwd("D:/R/data")
write.csv(my.state,"rich_state.csv",row.names=F)

ds <- read.csv("rich_state.csv",header=T)
ds


sum=0
count = 0
for(i in 1:100) {
  if(i%%3==0) {
    sum <-  sum + i
    count <-  count + 1
  }
}
print(sum)
print(count)

for(i in 100:200){
  if(i%%3==0 & i%%4==0){
    print(i)
  }
}

for (i in 1:24) {
  if (24%%i==0){
    print(i)
  }
}

result=1
for(i in 1:10){
  result <- result * i 
}
result

for(i in 1:100){
  if(i%%3==0)
    cat("*")
  else
    cat(i)
}

prime <- c()
for(i in 2:1000){
  count <- 0
  for(j in 2:i){
    if(i%%j==0){
      count <- count + 1
    }
  }
  if(count==1){
    prime[i] <- i
   }
}
prime <- prime[!is.na(prime)]
prime

for(i in 2:1000){
  
  na.omit(prime)
  cat(prime[i])
}





x <- 2
for(i in 3:1000){
  y <- i
  z <- c(2:(i-1))
  if(sum(which(y%%z==0))==0)
    x <- c(x,y)
}




for(i in 2:1000){
  d[i]
}
if(count==1){
  d <- c(i)
}

one <- 0
two <- 1
result <- -1
for(i in 2:40){
  cat(one)
  result <- one+two
  one <- two
  two <- result
  cat(two)
  cat(result )
}

lgm<- function(x,y){
  temp <- 1
  gcd <- x*y
  while(temp!=0){
    temp <- y%%x
    y <- x
    x <- temp
  }
  answer <- y
  return(answer)
}

lgm(10,8)

weight <- c(69, 50, 55, 71, 89, 64, 59, 70, 71, 80)

which.min(weight)




st2
st["Pennsylvania",]
st["Ohio",c("Population","Income")]
st2 <- subset(st,Illiteracy>=2.0)
st3 <- subset(st,max(Life.Exp))

st5 <- st["Pennsylvania","Income"]
st5
st6 <- subset(st,Income>st5)
rownames(st6)


mean(st2[,2])-mean(st3[,2])
st["Population","Income">4500,"Area"]
state.x77[1:2,c(1:2)]
head(state.x77)
setwd("D:/R")
my.iris2 <- subset(iris,Species=='setosa')
write.csv(my.iris2,"my_iris3.csv",row.names=T)
x <- 10:1
y <- -4:5
q <- c("ho","fb","bab","cur","rug","lac","bask","ten","cri","soc")
theDF <- data.frame(First=x,Second=y,Sprots=q,stringsAsFactors=FALSE)
theDF
install.packages("readr")
library(readr)
theURL <- "http://www.jaredlander.com/data/TomatoFirst.csv"
tomato2 <- read_delim(file=theURL,delim=",")
head(tomato2)
install.packages("readxl")
download.file(url="https://www.jaredlander.com/data/ExcelExample.xlsx",destfile="data/ExcelExample.xlsx",method="curl")
library(readxl)
excel_sheets("data/ExcelExample.xlsx")
tomatoXL <- read_excel("data/ExcelExample.xlsx")
tomatoXL
tomatoXL2 <- read_excel("data/ExcelExample.xlsx",sheet=2)
tomatoXL2
save(tomato2, file = "data/tomato2.rdata")
rm(tomato2)
load("data||tomato2.rdata")
n <- 20
r <- 1:20
w <- data.frame(n,r)
n
r
w
setwd("D:/R")
air <- read.csv()
save(n,r,w,file="data.multiple.rdata")
rm(n,r,w)
load("data/multiple.rdata")
test = c(15,20,30,NA,50)
test[test<40]
test[test%%3!= 0]
test[is.na(test)]
test[!is.na(test)]
test[test%%2==0&!is.na(test)]
characters=data.frame(name=c("±æµ¿","ÃáÇâ","Ã¶¼ö"),
                     age=c(30,16,21),
                     gender=factor(c("M","F","M")))
characters
characters[characters$gender == "F",]
characters[characters$age<30 & characters$gender=="M",]
job.type <- 'A'
if(job.type=='B')
{
  bonus <- 200
}else
{
  bonus <- 100
}
print(bonus)
d <- 100:200
d
d[-c(91:101)]
d.20 <- d[-c(1:20)]
d.20
d.20[-c(5,7,9)]
d[-c(3:5)]
d[seq(2,101,2)]
d[d%%3==0]
a <- 10
b <- 20
if(a>b)
{c <- a
} else
{
  c <- b
}
print(c)
c <- ifelse(a>b,a,b)
print(c)
x=c(-5:5)
options(digits=3)
sqrt(x)
sqrt(ifelse(x>=0,x,NA))
for(i in 1:10)
{check =0
  for(j in 1:i)
  {
   if(i%%j==0){
     check=check+1
   }
  }

if(check==2)
  print(i)
} 
norow <- nrow(iris)
mylabel <- c()
for(i in 1:norow)
{
  if(iris$Petal.Length[i]<=1.6)
  {
    mylabel[i] <- 'L'
  } else if(iris$Petal.Length[i]>=5.1){
    mylabel[i] <- 'H'
  } else {
    mylabel[i] <- 'M'
  }
}
print(mylabel)
newds <- data.frame(iris$Petal.Length,mylabel)
newds
v1 <- 51:90
v1 <- v1[v1%%2==0]*2
v1
v1[v1%%2==1|v1>80]
v1[v1%%3==0&v1%%5==0]

sum(v1[v1%%2==0])
sum(v1)
mymax <- function(x,y){
  num.max <- x
  if(y>x){
    num.max <- y
}
return(num.max)
}
a <- mymax(10,25)
a
myfunc <- function(x,y){
  val.sum <- x+y
  val.mul <- x*y
  result(list(sum=val.sum, mul=val.mul))
}
myfunc
result
score <- c(76,60,80,54,19,84,56,99)
which(score<=60)
idx(score<=60)



