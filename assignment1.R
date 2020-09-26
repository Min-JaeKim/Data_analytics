d <- 100:200
d

d[10]

d[-c(91:101)]

d[seq(2,101,2)]

sum(d[d%%3==0]) # pdf는 답 틀렸음

d.20 <- d[-c(1:20)]
d.20

d.20[-c(5)]

d.20[-c(5,7,9)]


# 2번

absent <- c(10,8,14,15,9,10,15,12,9,7,8,7)
names(absent) <- c("JAN", "FEB", "MAR", "APR", "MAY", "JUN", "JUL", "AUG", "SEP",
                   "OCT", "NOV", "DEC")
absent

absent["MAY"]

absent[c(7,9)]

sum(absent[1:6])

mean(absent[7:12])


#3번

d1 <- 1:50
d2 <- 51:100

d1
d2

length(d2)

# 3,4는 위에 나왔기 때문에 건너뜀

sum(d1+d2)

max(d2)
min(d2)

# 7도 중복

sort(d1,decreasing = TRUE)

d3 <- c(d1[1:10], d2[1:10]) # 2번에 9 답 수정
d3


#4번

v1 <- 51:90

v1[v1<60]

sum(v1<70)

sum(v1[v1>65])

v1[v1>60&v1<73]

v1[v1%%7==3]

v1[v1%%7==0]=0

sum(v1[v1%%2==0])

v1[v1%%2==1|v1>80]

v1[v1%%3==0&v1%%5==0]

v1[v1%%2==0] <- v1[v1%%2==0]*2

v1[v1%%7==0] <- NA
v1


#5번

st <- data.frame(state.x77)

st

colnames(st)

rownames(st)

dim(st) # 행의 갯수와 열의 갯수

str(st)

rowSums(st)
rowMeans(st)

colSums(st)
colMeans(st)

st["Florida",]

st[,"Income"]

st["Texas",8]

st["Ohio",c("Population","Income")]

subset(st,Population>5000)

st[rownames(subset(st,Income>=4500)),c("Population","Income","Area")] # 14수정

nrow(subset(st,Income>=4500))

subset(st,Area>=100000&Frost>=120)

subset(st,Population<2000&Murder<12)

mean(st[rownames(subset(st,Illiteracy>=2.0)),2])

mean(st[rownames(subset(st,Illiteracy<2.0)),2]) - 
  mean(st[rownames(subset(st,Illiteracy>=2.0)),2])

rownames(st[which.max(st$Life.Exp),])

rownames(subset(st,Income>st["Pennsylvania","Income"]))


# 6번

class(mtcars)

nrow(mtcars)
ncol(mtcars)

colnames(mtcars)

rownames(mtcars[which.max(mtcars$mpg),])

rownames(mtcars[which.min(mtcars$mpg),])

mtcars["Honda Civic",c("mpg","gear")]

rownames(subset(mtcars,mpg>mtcars["Pontiac Firebird","mpg"]))

mean(mtcars$mpg)

unique(mtcars[,"gear"])


# 7번

class(airquality)

head(airquality)

airquality[which.max(airquality$Temp),c("Month","Day")]

airquality[which.max(subset(airquality,Month==6)$Wind),]

mean(subset(airquality,Month==7)$Temp)

mean(subset(subset(airquality,Ozone!="NA"),Month==5)$Ozone)

nrow(subset(airquality,Ozone>100)) # 7번 7 수정


# 8번

str(swiss)

rownames(swiss[which.max(swiss$Agriculture),])

sort(swiss$Agriculture,decreasing = TRUE)

swiss[which(swiss$Catholic>=80),"Agriculture"]

swiss[which(swiss$Examination<20&swiss$Agriculture<50),c("Examination","Agriculture")]


# 9번

st <- data.frame(state.x77)
subset(st,Income>=5000)[,c("Income","Population","Area")]
setwd("D:/R/data")
write.csv(subset(st,Income>=5000),"rich_state2.csv",row.names=T)

ds <- read.csv("rich_state2.csv",header=T)
ds


# 10번

sum=0
count=0
for(i in 1:100) {
  if(i%%3==0) {
    sum <- sum+i
    count <- count +1
  }
}
sum
count

for(i in 100:200) {
  if(i%%3==0 & i%%4==0){
    print(i)
  }
}

for(i in 1:24){
  if(24%%i==0){
    print(i)
  }
}

result=1
for(i in 1:10){
  result <- result*i
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
      count <-  count +1
    }
  }
  if(count==1){
    prime[i] <- i
  }
}
prime[!is.na(prime)]

one <-  0 
two <- 1
result <- -1
for(i in 2:40){
  cat(one)
  result <-  one+two
  one <- two
  two <- result
  cat(two)
}
