#MA thesis
#Time Series Clustering
#Created by: Patryk Zieliński, WZ AGH

library(xlsx)
library(httr)
library(psych)
library(tidyr)
library(dplyr)
library(tseries)
library(ggplot2)
library(TSclust)
library(ggcorrplot)
library(dtwclust)
library(clValid)
library(dendextend)


# Data import ----

#data import
url <- "https://github.com/getry28/RProjects/blob/master/MAThesis_Data.xlsx?raw=true"
GET(url, write_disk("MAThesis_Data.xlsx", overwrite=TRUE))
Data2017 <- read.xlsx("MAThesis_Data.xlsx", sheetIndex=4, header=TRUE)
Data2018 <- read.xlsx("MAThesis_Data.xlsx", sheetIndex=5, header=TRUE)%>%
  na.omit()

labels<-colnames(Data2017%>%
                   select(-1))


# Descriptive statistics ----

#descriptive statistics - 2017
DS2017<-describe(Data2017%>%
                   select(-Data))%>%
  select(-c('vars','n','trimmed','mad','se'))
colnames(DS2017)<-c("Srednia","Odchylenie","Mediana","MIN","MAX","Rozstep",
                    "Skośność","Kurtoza")

#descriptive statistics - 2018
DS2018<-describe(Data2018%>%
                   select(-Data))%>%
  select(-c('vars','n','trimmed','mad','se'))
colnames(DS2018)<-c("Srednia","Odchylenie","Mediana","MIN","MAX","Rozstep",
                    "Skośność","Kurtoza")

#desriptive statisctics analysis - tables for export
DS2018A<-describe(DS2018)%>%
  select(-c('vars','n','trimmed','mad','se'))%>%
  round(digit=5)

DS2017A<-describe(DS2017)%>%
  select(-c('vars','n','trimmed','mad','se'))%>%
  round(digit=5)

#descriptive statistics 2017 boxplots
boxplot(DS2017$mean, DS2018$mean, horizontal=T, xlab="Wartości średniej",
        names=c("2017", "2018"), las=1, col=c("orange","red"),
        border="brown")

boxplot(DS2017$sd, DS2018$sd, horizontal=T, 
        xlab="Wartości odchylenia standardowego",
        names=c("2017", "2018"), las=1, col=c("orange","red"),
        border="brown")

boxplot(DS2017$skew, DS2018$skew, horizontal=T, 
        xlab="Wartości współczynnika skośności",
        names=c("2017", "2018"), las=1, col=c("orange","red"),
        border="brown")

boxplot(DS2017$Kurtoza, DS2018$Kurtoza, horizontal=T, xlab="Wartości kurtozy",
        names=c("2017", "2018"), las=1, col=c("orange","red"),
        border="brown")


# Autocorrelation analysis ----


#ACF - lag 1 - 2017
ACF2017<-acf(Data2017%>%
      select(-Data), lag.max=1, plot=F)

ACF1<-ACF2017[["acf"]][2,,]%>%
  as.data.frame()
colnames(ACF1)<-labels
DSACF2017<-describe(ACF1)%>%
  select(-c('vars','n','trimmed','mad','se'))
rownames(DSACF2017)<-labels

#ACF - lag 1 - 2018
ACF2018<-acf(Data2018%>%
               select(-Data), lag.max=1, plot=F)

ACF2<-ACF2018[["acf"]][2,,]%>%
  as.data.frame()
colnames(ACF2)<-labels
DSACF2018<-describe(ACF2)%>%
  select(-c('vars','n','trimmed','mad','se'))
rownames(DSACF2018)<-labels

#ACF significance test
ACFtest<-as.data.frame(matrix(0,68,4))
rownames(ACFtest)<-labels
colnames(ACFtest)<-c("2017","2018","Significance 2017","Significance 2018")

ACFtest$`2017`<-DSACF2017$mean*sqrt(NROW(DSACF2017$mean))
ACFtest$`2018`<-DSACF2018$mean*sqrt(NROW(DSACF2018$mean))

CIup<-1.96/sqrt(68)
CIdown<-(-1.96/sqrt(68))

ACFtest$`Significance 2017`<-ifelse(ACFtest$`2017`<CIup & ACFtest$`2017`>CIdown,1,0)
ACFtest$`Significance 2018`<-ifelse(ACFtest$`2018`<CIup & ACFtest$`2018`>CIdown,1,0)

#ACF boxplots
boxplot(DSACF2017$Srednia,DSACF2018$Srednia, horizontal=T, 
        xlab="Wartości współczynnika autokorelacji pierwszego rzędu",
        names=c("2017", "2018"), las=1, col=c("orange","red"),
        border="brown")

#ACF density plots
DD<-cbind(DSACF2017$mean,DSACF2018$mean)%>%
  as.data.frame()
colnames(DD)<-c("2017", "2018")
DD2<-gather(DD,Rok, ACF)

ggplot(DD2, aes(x=ACF, color=Rok))+
         geom_density(size=1.1)+
  ylab("Gęstość")+
  xlab("Wartości współczynnika autokorelacji pierwszego rzędu")


# TS Clustering 2017 ----

#COR distance
D1<-diss(Data2017%>% select(-Data),"COR")
D1H<-hclust(D1, method="ward.D")
plot(D1H,hang=-1, cex=0.8)

#correlation analysis
COR2017<-cor(Data2017%>%
               select(-Data))%>%
  round(digit=2)

COR2017I<-describe(COR2017)%>%
  select(-c('vars','n','trimmed','mad','se'))%>%
  round(digit=2)

ggplot(COR2017I, aes(x=mean))+
  geom_histogram(binwidth=0.005, color="black", fill="black")+
  geom_density(size=1.1, color="#2c7fb8")+
  xlab("Wartości współczynników korelacji")+
  ylab("Gęstość")

p.mat<-cor_pmat(COR2017)
ggcorrplot(COR2017, type="upper", outline.col="white", p.mat=p.mat)

#AR distance
D2<-diss(Data2017%>% select(-Data),"AR.PIC")
D2H<-hclust(D2,method="ward.D")
plot(D2H,hang=-1, cex=0.8)

#periodogram distance
D3<-diss(Data2017%>% select(-c(Data,PBG)),"PER")
D3H<-hclust(D3, method="ward.D")
plot(D3H,hang=-1, cex=0.8)

#group periodograms
#group 1
spectrum(Data2017%>%
           select(c(SYGNITY,INTER.SPORT,MZN.PROPERTY,OPEN.FINANCE,SKYLINE)))

#group 2
spectrum(Data2017%>%
           select(c(ELZAB,KRAKCHEMIA,GETIN.NOBLE.BANK,INDYKPOL,JSW,MBANK,LOTOS,X11.BIT,
                    BOS.BANK,BNP.PARIBAS,EUROTEL,CD.PROJEKT,MOSTOSTAL.WAWA)))

#group 3
spectrum(Data2017%>%
           select(c(DOM.DEVELOPMENT,ING,PEKAO.SA,GTC,WIRTUALNA.POLSKA,DEBICA,JW.CONSTRUCTION,
                    IDEA.BANK,ALCHEMIA,ZYWIEC,NEWAG,PRIME.CAR.MANAGEMENT,SKARBIEC.HOLDING,
                    ASSECO,MENNICA.POLSKA,AMICA,RAWLPLUG,CYFROWY.POLSAT,MCI.CAPITAL,NETIA,
                    PKO.BP,INTER.CARS,BANK.HANDLOWY,PZU)))

#group 4
spectrum(Data2017%>%
           select(c(GETIN.HOLDING,ENERGA,ALIOR,ATAL,CCC,SEKO,BANK.MILLENIUM,ORLEN,PKP.CARGO,
                    PGE,TAURON,ENEA,PGNIG,FERRO,ORANGE,K2.INTERNET,PFLEIDERER,GRUPA.AZOTY,
                    KOMPUTRONIK,WAWEL,AGORA,COMARCH,HENRYK.KANIA,KGHM,TARCZYNSKI)))


# TS Clustering 2018 ----

#COR distance
D10<-diss(Data2018%>% select(-Data),"COR")
D10H<-hclust(D10, method="ward.D")
plot(D10H,hang=-1, cex=0.8)

#correlation analysis
COR2018<-cor(Data2018%>%
               select(-Data))%>%
  round(digit=2)

COR2018I<-describe(COR2018)%>%
  select(-c('vars','n','trimmed','mad','se'))%>%
  round(digit=2)

ggplot(COR2018I, aes(x=mean))+
  geom_histogram(binwidth=0.005, color="black", fill="black")+
  geom_density(size=1.1, color="#2c7fb8")+
  xlab("Wartości współczynników korelacji")+
  ylab("Gęstość")

p.mat2<-cor_pmat(COR2018)
ggcorrplot(COR2018, type="upper", outline.col="white", p.mat=p.mat2)

#AR distance
D11<-diss(Data2018%>% select(-Data),"AR.PIC")
D11H<-hclust(D11,method="ward.D")
plot(D11H,hang=-1,cex=0.8)

#periodogram distance
D12<-diss(Data2018%>% select(-c(Data,PBG)),"PER")
D12H<-hclust(D12, method="ward.D")
plot(D12H,hang=-1, cex=0.8)

#group periodograms
#group 1
spectrum(Data2017%>%
           select(c(SYGNITY,INTER.SPORT,MZN.PROPERTY,OPEN.FINANCE,SKYLINE)))

#group 2
spectrum(Data2017%>%
           select(ELZAB,KRAKCHEMIA,GETIN.NOBLE.BANK,INDYKPOL,JSW,MBANK,LOTOS,X11.BIT,
                  BOS.BANK,BNP.PARIBAS,EUROTEL,CD.PROJEKT,MOSTOSTAL.WAWA))
#group 3
spectrum(Data2017%>%
           select(DOM.DEVELOPMENT,ING,PEKAO.SA,GTC,WIRTUALNA.POLSKA,DEBICA,JW.CONSTRUCTION,
                  IDEA.BANK,ALCHEMIA,ZYWIEC,PRIME.CAR.MANAGEMENT,SKARBIEC.HOLDING,ASSECO,MENNICA.POLSKA,
                  AMICA,RAWLPLUG,CYFROWY.POLSAT,MCI.CAPITAL,NETIA,PKO.BP,
                  INTER.CARS,BANK.HANDLOWY,PZU))

#group 4
spectrum(Data2017%>%
           select(SKYLINE,BNP.PARIBAS,MZN.PROPERTY,INTER.SPORT,HENRYK.KANIA,CD.PROJEKT,GRUPA.AZOTY,
                  MOSTOSTAL.WAWA,X11.BIT,KRAKCHEMIA,SYGNITY,ELZAB,PRIME.CAR.MANAGEMENT))


# TS Sq Clustering 2017 ----

Data2017SQ<-(Data2017%>%
  select(-Data))^2%>%
  round(digit=6)
Data2017SQ<-cbind(Data2017$Data,Data2017SQ)
names(Data2017SQ)[1]<-"Data"

D100<-diss(Data2017SQ%>% select(-Data),"COR")
D100H<-hclust(D100, method="ward.D")%>%
  plot(hang=-1, cex=1)

D101<-diss(Data2017SQ%>% select(-Data),"AR.PIC")
D101H<-hclust(D101, method="ward.D")%>%
  plot(hang=-1, cex=1)

D102<-diss(Data2017SQ%>% select(-c(Data,PBG)),"PER")
D102H<-hclust(D102, method="ward.D")%>%
  plot(hang=-1, cex=1)

# TS Sq Clustering 2018 ----
Data2018SQ<-(Data2018%>%
               select(-Data))^2%>%
  round(digit=6)
Data2018SQ<-cbind(Data2018$Data,Data2018SQ)
names(Data2018SQ)[1]<-"Data"

D110<-diss(Data2018SQ%>% select(-Data),"COR")
D110H<-hclust(D110, method="ward.D")%>%
  plot(hang=-1, cex=1)

D111<-diss(Data2018SQ%>% select(-Data),"AR.PIC")
D111H<-hclust(D111, method="ward.D")%>%
  plot(hang=-1, cex=1)

D112<-diss(Data2018SQ%>% select(-Data),"PER")
D112H<-hclust(D112, method="ward.D")%>%
  plot(hang=-1, cex=1)


# Dendrograms comparison ----

#correlation distance
dend_listCOR<-dendlist(as.dendrogram(D1H), as.dendrogram(D10H))

dendlist(as.dendrogram(D1H), as.dendrogram(D10H))%>%
  untangle(method="step1side")%>%
  tanglegram()

dendlist(as.dendrogram(D1H), as.dendrogram(D10H))%>%
  untangle(method="step1side")%>%
  entanglement()

cor.dendlist(dend_listCOR, method="cophenetic")

#AR distance
dend_listAR<-dendlist(as.dendrogram(D2H), as.dendrogram(D11H))

dendlist(as.dendrogram(D2H), as.dendrogram(D11H))%>%
  untangle(method="step1side")%>%
  tanglegram()

dendlist(as.dendrogram(D2H), as.dendrogram(D11H))%>%
  untangle(method="step1side")%>%
  entanglement()

cor.dendlist(dend_listAR, method="cophenetic")

#periodogram distance
dend_listPER<-dendlist(as.dendrogram(D3H), as.dendrogram(D12H))

dendlist(as.dendrogram(D3H), as.dendrogram(D12H))%>%
  untangle(method="step1side")%>%
  tanglegram()

dendlist(as.dendrogram(D3H), as.dendrogram(D12H))%>%
  untangle(method="step1side")%>%
  entanglement()

cor.dendlist(dend_listAR, method="cophenetic")
