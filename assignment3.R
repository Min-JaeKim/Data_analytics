score = c(77.5,60,50,95,55,85,72.5,80,92.5,87.5)
wtime <- c(14,10,20,7,25,9,15,13,4,21)
cor(score,wtime)


years <- 2015:2026
populations <- c(51014,51245,51446,51635,51811,51973,52123,52261,52388,52504,52609,52704)
plot(years,
     populations,
     main="월별지각생통계",
     type="l",
     lty=1,
     lwd=1,
     xlab="years",
     ylab="populations")


color=c("red","blue")
age <- Orange$age
circumference <- Orange$circumference
plot(age,circumference,
     main = "나이-둘레 그래프",
     xlab="age",
     ylab="circumference",
     col=color)



library(mlbench)
data("Glass")
myds <- Glass[,c("RI","Na","Mg","Al","Si","K","Ca","Ba","Fe")]
head(myds)
pairs(myds)
ri <- Glass$RI
na <- Glass$Na
mg <- Glass$Mg
al <- Glass$al
si <- Glass$Si
k <- Glass$K
ca <- Glass$Ca
ba <- Glass$Ba
fe <- Glass$Fe
col(myds)
color <- c("red","blue","green","yellow","purple","orange","coral","brown","beige")
pairs(myds,col=color)


month <-  1:12
years.1949 <-  window(AirPassengers, 1949, c(1949, 12))
years.1949 <- c(112,118,132,129,121,135,148,148,136,119,104,118)
years.1955 <-  window(AirPassengers, 1955, c(1955, 12))
years.1955 <- c(242,233,267,269,270,315,364,347,312,274,237,278)
years.1960 <-  window(AirPassengers, 1960, c(1960, 12))
years.1960 <- c(417,391,419,461,472,535,622,606,508,461,390,432)



month = 1:12
years.1949  <-  window(AirPassengers, 1949, c(1949, 12))
years.1955 <-  window(AirPassengers, 1955, c(1955, 12))
years.1960 <-  window(AirPassengers, 1960, c(1960, 12))
plot(month,
     years.1949,
     main = "passengers",
     type="b",
     lty=1,
     col="red",
     xlab="Month",
     ylab = "count",
     ylim=c(100,700))
lines(month,
      years.1955,
      type="b",
      col="blue")
lines(month,
      years.1960,
      type="b",
      col="coral")

month = 1:12
years.1949 = c(112,118,132,129,121,135,148,148,136,119,104,118)
years.1955 = c(242,233,267,269,270,315,364,347,312,274,237,278)
plot(month,
     years.1949,
     main = "passengers",
     type="b",
     lty=1,
     col="red",
     xlab="Month",
     ylab = "count",
     ylim=c(100,300))
lines(month,
      years.1955,
      type="b",
      col="blue")


month=1:12
late1=c(112,118,132,129,121,135,148,148,136,119,104,118)
late1
late2=c(242,233,267,269,270,315,364,347,312,274,237,278)
plot(month,
     late1,
     main="Late S",
     type="b",
     lty=1,
     col="coral",
     xlab="M",
     ylab = "L")
lines(month,
      late2,
      type="b",
      col="green")



ds <- state.x77
ds[2,3] <- NA; ds[3,1] <- NA; ds[2,4] <- NA; ds[4,3] <- NA
colSums(is.na(ds))
sum(rowSums(is.na(ds))>0)
ds.new <- ds[complete.cases(ds),]
ds.new



st <- data.frame(state.x77)
head(st)
boxplot(st$Population)
boxplot(st$Income)
boxplot(st$Illiteracy)
boxplot(st$Life.Exp)
boxplot(st$Murder)
boxplot(st$HS.Grad)
boxplot(st$Frost)
boxplot(st$Area)
out.val <- boxplot.stats(st$Population)$out
st$Population[st$Population %in% out.val] <- NA
out.val2 <- boxplot.stats(st$Area)$out
st$Area[st$Area %in% out.val2] <- NA
head(st)
st2 <- st[complete.cases(st),]
head(st2)


# 시험 연습

# 1번

score = c(77.5,60,50,95,55,85,72.5,80,92.5,87.5)
wtime <- c(14,10,20,7,25,9,15,13,4,21)
cor(score,wtime)

