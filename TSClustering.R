#MA thesis
#Time Series Clustering
#Created by: Patryk Zieliński, WZ AGH

library(xlsx)
library(httr)
library(psych)
library(dplyr)
library(tseries)
library(ggplot2)
library(TSclust)
library(dtwclust)

#data import
url <- "https://github.com/getry28/RProjects/blob/master/MAThesis_Data.xlsx?raw=true"
GET(url, write_disk("MAThesis_Data.xlsx", overwrite=TRUE))
Data2017 <- read.xlsx("MAThesis_Data.xlsx", sheetIndex=4, header=TRUE)
Data2018 <- read.xlsx("MAThesis_Data.xlsx", sheetIndex=5, header=TRUE)%>%
  na.omit()

#PBG wywalić
#do roboty: statystyki opisowe ze statystyk opisowych
# + współczynniki autokorelacji 1-go rzędu

labels<-colnames(Data2017%>%
                   select(-1))

#descriptive statistics - 2017
DS2017<-describe(Data2017%>%
                   select(-Data))%>%
  select(-c('vars','n','trimmed','mad','se'))

#descriptive statistics - 2018
DS2018<-describe(Data2018%>%
                   select(-Data))%>%
  select(-c('vars','n','trimmed','mad','se'))

#descriptive statistics 2017 boxplots
#chyba że zrobić np. histogram z funkcją gęstości - DO DOPYTANIA
boxplot(DS2017$mean, horizontal=T, xlab="Wartości średniej")

boxplot(DS2017$sd, horizontal = T, xlab="Wartości odchylenia standardowego")

#boxplot(DS2017$median, horizontal=T, xlab="Wartości mediany")

boxplot(DS2017$skew, horizontal=T, xlab="Wartości współczynnika skośności")

boxplot(DS2017$kurtosis, horizontal=T, xlab="Wartości kurtozy")

#ACF - lag 1 - 2017
ACF2017<-acf(Data2017%>%
      select(-Data), lag.max=1, plot=F)

ACF1<-ACF2017[["acf"]][2,,]%>%
  as.data.frame()
colnames(ACF1)<-labels
DSACF2017<-describe(ACF1)%>%
  select(-c('vars','n','trimmed','mad','se'))
rownames(DSACF2017)<-labels

boxplot(DSACF2017$mean, horizontal=T, xlab="Wartości współczynnika autokorelacji pierwszego rzędu")

#descriptice statitstics 2018 boxplots
boxplot(DS2018$mean, horizontal=T, xlab="Wartości średniej")

boxplot(DS2018$sd, horizontal = T, xlab="Wartości odchylenia standardowego")

#boxplot(DS2018$median, horizontal=T, xlab="Wartości mediany")

boxplot(DS2018$skew, horizontal=T, xlab="Wartości współczynnika skośności")

boxplot(DS2018$kurtosis, horizontal=T, xlab="Wartości kurtozy")

#ACF - lag 1 - 2018
ACF2018<-acf(Data2018%>%
               select(-Data), lag.max=1, plot=F)

ACF2<-ACF2018[["acf"]][2,,]%>%
  as.data.frame()
colnames(ACF2)<-labels
DSACF2018<-describe(ACF2)%>%
  select(-c('vars','n','trimmed','mad','se'))
rownames(DSACF2018)<-labels

boxplot(DSACF2018$mean, horizontal=T, xlab="Wartości współczynnika autokorelacji pierwszego rzędu")

#TS clustering - 2017
#COR distance
D1<-diss(Data2017%>% select(-Data),"COR")
D1H<-hclust(D1, method="ward.D")%>%
  plot(hang=-1, cex=0.8)


#AR distance
D2<-diss(Data2017%>% select(-Data),"AR.MAH")
D2H<-hclust(D2[["p_value"]],method="ward.D")
D2H[["labels"]]<-labels
plot(D2H,hang=-1, cex=0.8)

#periodogram distance
D3<-diss(Data2017%>% select(-c(Data,PBG)),"PER")
D3H<-hclust(D3, method="ward.D")%>%
  plot(hang=-1, cex=0.8)


#TS clustering - 2018
#COR distance
D10<-diss(Data2018%>% select(-Data),"COR")
D10H<-hclust(D10, method="ward.D")%>%
  plot(hang=-1, cex=0.8)

#AR distance
D11<-diss(Data2018%>% select(-Data),"AR.MAH")
D11H<-hclust(D11[["p_value"]],method="ward.D")
D11H[["labels"]]<-labels
  plot(D11H, hang=-1,cex=0.8)

#periodogram distance
D12<-diss(Data2018%>% select(-c(Data,PBG)),"PER")
D12H<-hclust(D12, method="ward.D")%>%
  plot(hang=-1, cex=0.8)


