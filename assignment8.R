mydata<-read.csv("https://stats.idre.ucla.edu/stat/data/binary.csv")
head(mydata)
mydata$admit <- as.integer(mydata$admit)
head(mydata)
mod.mydata <- glm(admit ~., data=mydata)
summary(mod.mydata)
mod.mydata


unknown <- mydata[,c(2,3,4)]
head(unknown)
preq <- predict(mod.mydata,unknown)
preq <- round(preq,0)
preq
answer <- as.integer(mydata$admit)
preq == answer
acc <- mean(preq == answer)
acc



library(mlbench)
data(PimaIndiansDiabetes)
set.seed(100)
train <- sample(60,replace = TRUE)
test <- sample(40,replace = TRUE)
train <- data.frame(train)
test <- data.frame(test)
PimaIndiansDiabetes.12 <- cbind(PimaIndiansDiabetes.new$diabetes,train)
head(PimaIndiansDiabetes.12)

head(PimaIndiansDiabetes)
PimaIndiansDiabetes.new <- PimaIndiansDiabetes
PimaIndiansDiabetes.new$diabetes <- as.integer(PimaIndiansDiabetes.new$diabetes)
head(PimaIndiansDiabetes.new)
mod.PimaIndiansDiabetes <- glm(diabetes ~ train, data=PimaIndiansDiabetes.12)




ds <- PimaIndiansDiabetes
ds$diabetes <- as.integer(ds$diabetes) # 팩터를 숫자로 변환
set.seed(100)
idx <- sample(1:nrow(ds), 0.6*nrow(ds))
train <- ds[idx,]
test <- ds[-idx,]

mod <- glm(diabetes~., data=train)
summary(mod)
mod
#diabates = (1.638e-01) + (1.269e-02*train$pregnant)
#           +(5.693e-03*train$glucose) + (-2.325e-03*train$pressure)
#           +(-4.888e-04*train$triceps) + (-7.771e-05+train$insulin)
#           +(1.380e-02+train$mass) + (1.206e-01*train$pedigree)
#           +(3.925e-03*train$age)

head(test)
mod2 <- glm(diabetes ~., data=test)
pred2 <- predict(mod2,test)
pred2 <- round(pred2,0)
pred2
answer2 <- as.integer(test$diabetes)
pred2 == answer
acc2 <- mean(pred2==answer2)
acc2





library(mlbench)
data(Glass)

head(Glass)
ds3 <- Glass
ds3$Type <- as.integer(ds3$Type)
idx3 <- sample(1:nrow(ds3), 0.6*nrow(ds3))
train <- ds3[idx3,]
test <- ds3[-idx3,]


mod3 <- glm(Type~., data=train)
summary(mod3)
mod3
#Type = (-91.2905) + (-13.2881*train$RI) + (1.4698*train$Na)
#       + (0.3682*train$Mg) + (1.7038*train$Al) + (1.1369*train$Si)
#       +(0.7841*train$K) + (0.8461*train$Ca)
#       +(1.3107*train$Ba) + (0.1437*train$Fe)

mod3 <- glm(Type ~., data=test)
pred3 <- predict(mod3,test)
pred3 <- round(pred3,0)
pred3
answer3 <- as.integer(test$Type)
pred3 == answer3
acc3 <- mean(pred3==answer3)
acc3