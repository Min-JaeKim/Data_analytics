install.packages("e1071")
library(randomForest)
install.packages(colon)
library(colon)
head(colon)
ucla=read.csv('http://stats.idre.ucla.edu/stat/data/binary.csv')
str(ucla)
library(survival)
clean_colon=na.omit(colon)
ucla$admit=factor(ucla$admit)
library(e1071)
head(ucla)
s=svm(admit~., data = ucla)
print(s)
s1 <- table(predict(s,ucla), ucla$admit)
r=rpart(admit~., data=ucla)
par(mfrow=c(1,1), xpd=NA)
plot(r)
text(r,use.n=TRUE)
p=predict(r,ucla,type='class')
table(p,ucla$admit)
f=randomForest(admit~., data=ucla)
plot(f)
print(f)
head(iris)
head(ucla)


library(survival)
head(colon)
clean_colon = na.omit(colon)
clean_colon$status = factor(clean_colon$status)
head(clean_colon)
s2=svm(status~rx + sex + age + obstruct + perfor + adhere + nodes + 
         differ + extent + surg + node4, data = clean_colon)
table(predict(s2,clean_colon),clean_colon$status)



clean_colon = na.omit(colon)
clean_colon=clean_colon[c(TRUE, FALSE), ]
str(clean_colon)

r3=rpart(status~rx + sex + age + obstruct + perfor + adhere + nodes + 
           differ + extent + surg + node4, data = clean_colon)
p3=predict(r3, clean_colon, type = 'class')
table(p3,clean_colon$status)
plot(r3)
text(r3, use.n=TRUE)


p3=predict(r2,clean_colon,type='class')
table(p3,clean_colon$status)

f3 = randomForest(status~rx + sex + age + obstruct + perfor + adhere + nodes + 
                    differ + extent + surg + node4, data = clean_colon)
print(f3)

voice = read.csv('D:/R/voice.csv')
head(voice)
s3 = svm(label~., data = voice)

library(mlbench)
head(Sonar)



library(class)
data("Sonar")
tr.idx <- c(seq(from=1, to=208, by=2))
ds.tr <- Sonar[tr.idx, 1:60]
ds.ts <- Sonar[-tr.idx, 1:60]
cl.tr <- factor(Sonar[tr.idx, 61])
cl.ts <- factor(Sonar[-tr.idx, 61])
pred3 <- knn(ds.tr, ds.ts, cl.tr, k=3, prob = TRUE)
pred3
acc <- mean(pred3==cl.ts) 
acc
pred5 <- knn(ds.tr, ds.ts, cl.tr, k=5, prob = TRUE)
pred5
acc <- mean(pred5==cl.ts)
acc
pred7 <- knn(ds.tr, ds.ts, cl.tr, k=7, prob = TRUE)
pred7
acc <- mean(pred7==cl.ts) 
acc



str(kyphosis) 
r = rpart(Kyphosis~., data=kyphosis) 
plot(r) 
text(r, use.n = TRUE) 
p = predict(r, kyphosis, type = 'class')
table(p, kyphosis$Kyphosis) 
f = randomForest(Kyphosis~. , data = kyphosis)
print(f)
tr.idx <- c(1:40)
ds.tr <- kyphosis[tr.idx, 2:4] 
ds.ts <- kyphosis[-tr.idx, 2:4]
cl.tr <- kyphosis[tr.idx,1]
cl.ts <- kyphosis[-tr.idx,1] 
pred <- knn(ds.tr, ds.ts, cl.tr, k = 3, prob = TRUE) 
acc <- mean(pred == cl.ts)
acc


s = svm(Kyphosis~., data = kyphosis)
table(predict(s, kyphosis), kyphosis$Kyphosis)

install.packages(cvTools)
install.packages('cvTools')
library(cvTools)
k = 5 
folds <- cvFolds(nrow(iris), K=k)
acc <- c()
for(i in 1:k){ 
  +   ts.idx <- folds$which == i 
  +   ds.tr <- Sonar[-ts.idx, 1:60] 
  +   ds.ts <- Sonar[ts.idx, 1:60] 
  +   cl.tr <- factor(Sonar[-ts.idx, 61]) 
  +   cl.ts <- factor(Sonar[ts.idx, 61]) 
  +  
    +   pred <- knn(ds.tr, ds.ts, cl.tr, k = 3) 
    +   acc[i] <- mean(pred==cl.ts) 
    + }