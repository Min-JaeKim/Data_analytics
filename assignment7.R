head(state.x77)
st <- data.frame(state.x77)
head(cars)
plot(Murder~Illiteracy, data=st)
model1 <- lm(Murder~Illiteracy,st)
abline(model1)
b <- coef(model1)[1]
w <- coef(model1)[2]
Illteracy <- 0.5
Murder <- w*Illteracy + b
Murder
Illteracy <- 1.0
Murder <- w*Illteracy + b
Murder
Illteracy <- 1.5
Murder <- w*Illteracy + b
Murder


head(trees)
plot(Volume~Girth, data=trees)
model2 <- lm(Volume~Girth,trees)
abline(model2)
b <- coef(model2)[1]
w <- coef(model2)[2]
Girth <- 8.5
Volume <- w*Girth + b
Volume
Girth <- 9.0
Volume <- w*Girth + b
Volume
Girth <- 9.5
Volume <- w*Girth + b
Volume


head(pressure)
plot(pressure~temperature, data=pressure)
model3 <- lm(pressure~temperature,pressure)
abline(model3)
b <- coef(model3)[1]
w <- coef(model3)[2]
temperature <- 65
pressure <- w*temperature + b
pressure
temperature <- 95
pressure <- w*temperature + b
pressure
temperature <- 155
pressure <- w*temperature + b
pressure


head(trees)
newdata1 <- trees[,c(1:3)]
plot(newdata1,pch=16,col="blue",
     main="treesPlot")
model4 <- lm(Volume~Girth + Height, data=trees)
summary(model4)
predict(model4)

fitted(model4)
residuals(model4)
deviance(model4)


install.packages("mlbench")
library(mlbench)
data(BostonHousing)
head(BostonHousing)
model5 <- lm(medv~ . , data=BostonHousing)
model5
#medv = (3.646e+01) + (-1.080e-01*BostonHousing$crim) + (4.642e-02*BostonHousing$zn) + (2.056e-02*BostonHousing$indus)
#       +(2.687e+00*BostonHousing$chas1) + (-1.777e+01*BostonHousing$nox) + (3.810e+00*BostonHousing$rm) + (6.922e-04*BostonHousing$age)
#       +(-1.476e+00*BostonHousing$dis) + (3.060e-01*BostonHousing$rad) + (-1.233e-02*BostonHousing$tax)
#       +(-9.527e-01*BostonHousing$ptratio) + (9.312e-03*BostonHousing$b) + (-5.248e-01*BostonHousing$lstat)
library(MASS)
model5.1 <- stepAIC(model5)
model5.1
#medv = (36.341145) + (-0.108413*BostonHousing$crim) + (0.045845*BostonHousing$zn) 
#       +(2.718716*BostonHousing$chas1) + (-17.376023*BostonHousing$nox) + (3.801579*BostonHousing$rm)
#       +(-1.492711*BostonHousing$dis) + (0.299608*BostonHousing$rad) + (-0.011778*BostonHousing$tax)
#       +(-0.946525*BostonHousing$ptratio) + (0.009291*BostonHousing$b) + (-0.522553*BostonHousing$lstat)
summary(model5)
summary(model5.1)



model6 <- lm(mpg ~ . , data = mtcars)
model6
model6.1 <- stepAIC(model6)
model6.1
summary(model6)
summary(model6.1)
