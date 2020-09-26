santa<-data.frame(belief=c('nobelief','nobelief','nobelief','nobelief',
                           'belief','belief','belief','belief',
                           'belief','belief','nobelief','nobelief',
                           'belief','belief','nobelief','nobelief'),
                  sibling=c('olderbrother','olderbrother',
                            'olderbrother','oldersister',
                            'nooldersibling','nooldersibling',
                            'nooldersibling','oldersister',
                            'olderbrother','oldersister',
                            'olderbrother','oldersister',
                            'nooldersibling','oldersister',
                            'olderbrother','nooldersibling')
)
head(santa)
mosaicplot(~belief+sibling,data = santa, color = TRUE)


library(ggplot2)
ggplot(mtcars,aes(x=mpg)) + 
         geom_histogram(binwidth=5.0)


library(ggplot2)
ggplot(trees,aes(x=Girth)) + 
  geom_histogram(binwidth=3.0,
                 fill="steelblue") +
  ggtitle("나무에 대한 나무 둘레") + 
  labs(x="나무들", y="나무 둘레")


library(ggplot2)
ggplot(data=mtcars, aes(x=mpg, y=wt,
                        color=gear)) +
  geom_point()




head(mtcars)
head(iris)
str(mtcars)
str(iris)
library(dplyr)
install.packages("dplyr")
library()

mtcars <- dplyr::filter(mpg > 20)


library(ggplot2)
ggplot(data=mtcars, aes(y=mpg, fill=factor(cyl))) +
  geom_boxplot()

library(ggplot2)
ggplot(data=iris, aes(y=Petal.Length, fill=Species)) +
  geom_boxplot()






library(ggplot2)
year <- c(20144,20151,20152,20153,20154,20161,20162,20163,20164,20171,20172,20173)
men <- c(73.9,73.1,74.4,74.2,73.5,73,74.2,74.5,73.8,73.1,74.5,74.2)
women <- c(51.4,50.5,52.4,52.4,51.9,50.9,52.6,52.7,52.2,51.5,53.2,53.1)
df <- data.frame(year,men,women)
head(df)
ggplot(data=df,aes(x=year,y=men))+
  geom_line(col="red")+
  geom_line(aes(x=year,y=women) col="blue")
  
ggplot(data=df,aes(x=year,y=men))+
  geom_line(color="red")



gg <- ggplot(data=df,aes(x=year,y=men))
gg+
  geom_line(color="blue")+
  geom_line(aes(x=year,y=women),color="red")



library(ggplot2)
years <- 1937:1960
cnt <- as.vector(airmiles)
df2 <- data.frame(years,cnt)
ggplot(data=df2, aes(x=years,y=cnt)) +
  geom_line(col="red")


year <- c(20144,20151,20152,20153,20154,20161,20162,20163,20164,20171,20172,20173)
men <- c(73.9,73.1,74.4,74.2,73.5,73,74.2,74.5,73.8,73.1,74.5,74.2)
women <- c(51.4,50.5,52.4,52.4,51.9,50.9,52.6,52.7,52.2,51.5,53.2,53.1)
df <- data.frame(year,men,women)
gg <- ggplot(data=df,aes(x=year,y=men))
gg+
  geom_line(color="blue")+
  geom_line(aes(x=year,y=women),color="red")