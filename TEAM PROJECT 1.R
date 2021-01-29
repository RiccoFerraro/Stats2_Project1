
library(dplyr)
library(knitr)
library(ggplot2)
library(dplyr)
library(class)
library(caret)


setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
CaseData = read.csv('data1.csv',header = T,sep = ",")

CaseData %>% ggplot(aes(x=Year, y=MSRP, fill = Vehicle.Style)) + geom_boxplot() + theme_dark() 

CaseData %>% ggplot(aes(x=Year, y=MSRP, fill = Vehicle.Style)) + geom_boxplot() + theme_dark() 

CaseData %>% ggplot(aes(x=Engine.Cylinders, y=MSRP,color=Vehicle.Size))+ geom_point() + facet_grid(~Driven_Wheels) + theme_dark()


remove_missing(CaseData)

classifyDATA <- CaseData[,c(3,5,6,9,13,14,15)]
classifyDATA$Response <- CaseData[,c(16)]

glimpse(classifyDATA)


library(corrplot)
library(RColorBrewer)
M <- cor(classifyDATA)

corrplot(M, col=brewer.pal(n=6, name="RdYlBu"),tl.cex = .75)
