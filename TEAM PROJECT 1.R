
library(dplyr)
library(knitr)
library(ggplot2)
library(dplyr)
library(class)
library(caret)


setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
CaseData = read.csv('data1.csv',header = T,sep = ",")
CaseData = data.frame(CaseData, stringsAsFactors = TRUE)

CaseData <- filter(CaseData, MSRP < 100000)

CaseData %>% ggplot(aes(x=Year, y=MSRP, fill = Vehicle.Style)) + geom_boxplot() + theme_dark() 

CaseData %>% ggplot(aes(x=Year, y=MSRP, fill = Vehicle.Style)) + geom_boxplot() + theme_dark()  facet_wrap(~Driven_Wheels) 

CaseData %>% ggplot(aes(x=Engine.Cylinders, y=MSRP,color=Year))+ geom_point() + facet_wrap(~Driven_Wheels) + theme_dark()


CaseData %>% ggplot(aes(x=Engine.Cylinders, y=MSRP,color=Year))+ geom_point()+ theme_dark()

remove_missing(CaseData)

classifyDATA <- CaseData[,c(3,5,6,9,13,14,15)]
classifyDATA$Response <- CaseData[,c(16)]

glimpse(classifyDATA)


library(corrplot)
library(RColorBrewer)
M <- cor(CaseData)

corrplot(M, col=brewer.pal(n=6, name="RdYlBu"),tl.cex = .75)


sapply(CaseData, function(x) length(unique(x)))

count_pct <- function(df) {
  return(
    df %>%
      tally %>% 
      mutate(n_pct = 100*n/sum(n))
  )
}

CaseData %>% 
  group_by(Transmission.Type) %>% 
  count_pct


CaseData %>% 
  group_by(Driven_Wheels) %>% 
  count_pct

CaseData %>% 
  group_by(Number.of.Doors) %>% 
  count_pct


CaseData %>% 
  group_by(Vehicle.Size) %>% 
  count_pct

CaseData %>% 
  group_by(Engine.Fuel.Type) %>% 
  count_pct


CaseData %>% 
  group_by(Vehicle.Style) %>% 
  count_pct


colnames(is.na(CaseData))


#CHANGE TO THE IS 0 , NA 
which (is.na(CaseData))




CaseData %>%
  summarise_all(funs(sum(is.na(.))))


library(e1071) 
skewness(CaseData$MSRP)

skewness(CaseData$Popularity)

CaseData$Popularity2 <- as.factor(CaseData$Popularity)

CaseData %>% ggplot(mapping = aes(x=Popularity)) + geom_histogram(binwidth =250 )

CaseData %>% ggplot(mapping = aes(x=Popularity2, fill = Market.Category)) + geom_histogram(binwidth =250 )

glimpse(CaseData)

CaseData %>% ggplot(mapping = aes(x=MSRP)) + geom_histogram(binwidth =250 )


model.fit<-aov(MSRP~Popularity2,data=CaseData)
#par(mfrow=c(2,2))
#plot(model.fit$fitted.values,model.fit$residuals,ylab="Resdiduals",xlab="Fitted")
#qqnorm(model.fit$residuals)


par(mfrow=c(2,2))
plot(model.fit)



model2.fit<-lm(MSRP~Popularity2,data=CaseData)
#par(mfrow=c(2,2))
#plot(model.fit$fitted.values,model.fit$residuals,ylab="Resdiduals",xlab="Fitted")
#qqnorm(model.fit$residuals)


par(mfrow=c(2,2))
plot(model2.fit)



mysummary<-function(x){
  result<-c(length(x),mean(x),sd(x),sd(x)/length(x),min(x), max(x), IQR(x))
  names(result)<-c("N","Mean","SD","SE","MIN", "MAX","IQR")
  return(result)
}
sumstats<-aggregate(MSRP~Cylinders,data=CaseData,mysummary)
sumstats<-cbind(sumstats[,1:2],sumstats[,-(1:2)])

library(ggplot2)
ggplot(sumstats,aes(x=DepthCat,y=Mean,group=Strata,colour=Strata))+
  ylab("Plot")+
  geom_line()+
  geom_point()+
  geom_errorbar(aes(ymin=Mean-SE,ymax=Mean+SE),width=.1)


