head(state.x77)
str(state.x77)

st <- data.frame(state.x77)
std <- function(X) {
  return((X-min(X)) /(max(X)-min(X)))
}
mydata <- apply(st,2,std)
fit <- kmeans(x=mydata,centers=5)
fit
library(cluster)
clusplot(mydata, fit$cluster, color=TRUE, shade= TRUE,
         labels=2,lines=0)


library(mlbench)
data("Sonar")
head(Sonar)
mydata2 <- Sonar[,-61]
fit <- kmeans(x=mydata2,centers=2)
fit
clusplot(mydata2, fit$cluster, color=TRUE, shade=TRUE,
         labels=2, lines=0)


library(rpart)
head(iris.data)
iris.data <- iris[,c(1,2,4,5)]

r = rpart(Species~., data=iris.data)
r
par(mfrow = c(1,1),xpd=NA)
plot(r)
text(r,use.n = TRUE)
p=predict(r,iris.data,type='class')
table(p,iris$Species)

r_prior <- rpart(Species~., data=iris.data, parms = list(prior = c(0.1, 0.8, 0.1)))
plot(r_prior)
text(r_prior,use.n=TRUE)
