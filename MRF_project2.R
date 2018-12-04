#MRF - projekt 2
#Opracowanie: Mateusz Feć, Patryk Zieliński

library(dplyr)
library(ggplot2)
library(ggthemes)
library(tidyr)
library(psych)
library(randtests)
library(nortest)
library(tseries)

#spółka WIG20 - Orange PL
#spółka mWIG40 - GTC
GTCMonth<-read.csv("https://raw.githubusercontent.com/getry28/RProjects/master/gtc_m.csv",
                   header=T,sep=",")
GTCWeek<-read.csv("https://raw.githubusercontent.com/getry28/RProjects/master/gtc_w.csv",
                  header=T,sep=",")
OrangeMonth<-read.csv("https://raw.githubusercontent.com/getry28/RProjects/master/opl_m.csv",
                      header=T,sep=",")
OrangeWeek<-read.csv("https://raw.githubusercontent.com/getry28/RProjects/master/opl_w.csv",
                     header=T,sep=",")

# Analiza tygodniowych stóp zwrotu ----------------------------------------

#zadanie 1 - tygodniowe stopy zwrotu
spolki<-c("Orange","GTC")
TygodnioweStopyZwrotu<-matrix(0,nrow(OrangeWeek)-1,2)%>%
  as.data.frame()
colnames(TygodnioweStopyZwrotu)<-spolki

TygodnioweStopyZwrotu$Orange<-log((OrangeWeek$Zamkniecie[2:nrow(OrangeWeek)]/
                                 OrangeWeek$Zamkniecie[1:(nrow(OrangeWeek)-1)]))
TygodnioweStopyZwrotu$GTC<-log((GTCWeek$Zamkniecie[2:nrow(GTCWeek)]/
                              GTCWeek$Zamkniecie[1:(nrow(GTCWeek)-1)]))
TygodnioweStopyZwrotu<-cbind(OrangeWeek$Data[2:nrow(OrangeWeek)],TygodnioweStopyZwrotu)
names(TygodnioweStopyZwrotu)[1]<-"Data"
TygodnioweStopyZwrotu$Data<-as.Date(TygodnioweStopyZwrotu$Data)

#wykresy szeregów czasowych
LogWeek<-TygodnioweStopyZwrotu%>%
  gather(Spolka,StopaZwrotu,Orange:GTC)

ggplot(LogWeek,aes(x=Data,y=StopaZwrotu))+
  geom_line(aes(color=Spolka),size=1.1)+
  ggtitle("Wykresy tygodniowych logarytmicznych stóp zwrotu")+
  theme_economist(dkpanel=T)+
  theme(plot.title = element_text(hjust=0.5))

#testy stacjonarności
adf.test(TygodnioweStopyZwrotu$Orange,alternative="stationary")
kpss.test(TygodnioweStopyZwrotu$Orange)

adf.test(TygodnioweStopyZwrotu$GTC,alternative="stationary")
kpss.test(TygodnioweStopyZwrotu$GTC)

#statystyki opisowe
TygodnioweStatystyki<-describe(TygodnioweStopyZwrotu%>%
                                 select(Orange:GTC))%>%
  select(-c('vars','n','trimmed','se'))%>%
  t()

#wykresy pudełkowe
ggplot(LogWeek,aes(x=as.factor(Spolka),y=StopaZwrotu))+
  geom_boxplot(fill="#4271AE",colour="#1F3552",alpha=0.7,
               outlier.colour = "#1F3552",outlier.shape = 22)+
  scale_x_discrete(name="Spółka")+
  ggtitle("Wykresy pudełkowe tygodniowych logarytmicznych stóp zwrotu")+
  theme_economist(dkpanel=T)+
  theme(plot.title = element_text(hjust=0.5))

#test Walda-Wolfowitza na losowość
runs.test(TygodnioweStopyZwrotu$Orange)
runs.test(TygodnioweStopyZwrotu$GTC)

#tesy normalności
shapiro.test(TygodnioweStopyZwrotu$Orange)
shapiro.test(TygodnioweStopyZwrotu$GTC)

lillie.test(TygodnioweStopyZwrotu$Orange)
lillie.test(TygodnioweStopyZwrotu$GTC)

ggplot(LogWeek,aes(x=StopaZwrotu))+
  geom_density(aes(color=Spolka))+
  theme_economist(dkpanel = T)+
  ggtitle("Wykresy gęstości tygodniowych logarytmicznych stóp zwrotu")

ggplot(LogWeek,aes(sample=StopaZwrotu))+
  stat_qq(aes(color=Spolka))+
  theme_economist()+
  ggtitle("Wykresy kwantyl-kwantyl tygodniowych logarytmicznych stóp zwrotu")

#testy autokorelacji
ACFOrangeWeek<-acf(TygodnioweStopyZwrotu$Orange,plot=F)
ACFOrangeWeekdf<-with(ACFOrangeWeek,data.frame(lag,acf))

ACFGTCWeek<-acf(TygodnioweStopyZwrotu$GTC,plot=F)
ACFGTCWeekdf<-with(ACFGTCWeek,data.frame(lag,acf))

ggplot(ACFOrangeWeekdf,aes(lag,acf))+
  geom_bar(stat="identity",position="identity",fill="#4271AE",colour="#1F3552")+
  ggtitle("Korelogram logarytmicznych tygodniowych stóp zwrotu dla spółki Orange")+
  theme_economist(dkpanel=T)+
  theme(plot.title = element_text(hjust=0.5))+
  xlab("Opóźnienie")+
  ylab("Wartość funkcji ACF")+
  scale_x_continuous(breaks=seq(0,25,by=5))

ggplot(ACFGTCWeekdf,aes(lag,acf))+
  geom_bar(stat="identity",position="identity",fill="#4271AE",colour="#1F3552")+
  ggtitle("Korelogram logarytmicznych tygodniowych stóp zwrotu dla spółki GTC")+
  theme_economist(dkpanel=T)+
  theme(plot.title = element_text(hjust=0.5))+
  xlab("Opóźnienie")+
  ylab("Wartość funkcji ACF")+
  scale_x_continuous(breaks=seq(0,25,by=5))

#test Ljung-Boxa
LBResult<-matrix(0,5,2)%>%
  as.data.frame()
colnames(LBResult)<-spolki

for(i in 1:5){
  LBResult$Orange[i]<-Box.test(TygodnioweStopyZwrotu$Orange,type="Ljung-Box",lag = i)$p.value
  LBResult$GTC[i]<-Box.test(TygodnioweStopyZwrotu$GTC,type="Ljung-Box",lag = i)$p.value
}

# Analiza tygodniowych wolumenów obrotu -----------------------------------

#własności szeregów czasowych tygodniowego wolumenu obrotu
TygodniowyWolumen<-data.frame(OrangeWeek$Data,OrangeWeek$Wolumen,GTCWeek$Wolumen)
colnames(TygodniowyWolumen)<-c("Data",spolki)
TygodniowyWolumen$Data<-as.Date(TygodniowyWolumen$Data)

#wykresy szeregów czasowych
ggplot(TygodniowyWolumen,aes(Data,Orange))+
  geom_line(colour="#4271AE")+
  theme_economist(dkpanel=T)+
  ggtitle("Wykres szeregu czasowego tygodniowego wolumenu spółki Orange")+
  theme(plot.title = element_text(hjust=0.5))+
  ylab("Wartości tygodniowego wolumenu")

ggplot(TygodniowyWolumen,aes(Data,GTC))+
  geom_line(colour="#4271AE")+
  theme_economist(dkpanel=T)+
  ggtitle("Wykres szeregu czasowego tygodniowego wolumenu spółki GTC")+
  theme(plot.title = element_text(hjust=0.5))+
  ylab("Wartości tygodniowego wolumenu")

#testy stacjonarności
adf.test(TygodniowyWolumen$Orange,alternative="stationary")
kpss.test(TygodniowyWolumen$Orange)

adf.test(TygodniowyWolumen$GTC,alternative = "stationary")
kpss.test(TygodniowyWolumen$GTC)

#wykresy pudełkowe wolumenów
ggplot(TygodniowyWolumen,aes(y=Orange))+
  geom_boxplot()

ggplot(TygodniowyWolumen,aes(y=GTC))+
  geom_boxplot()

#testy zgodności z rozkładem normalnym
ks.test(TygodniowyWolumen$Orange,"pnorm")
ks.test(TygodniowyWolumen$GTC,"pnorm")

#testy zgodności z rozkładem lognormalnym
ks.test(TygodniowyWolumen$Orange,"plnorm")
ks.test(TygodniowyWolumen$GTC,"plnorm")

#badanie autokorelacji
WolumenWeekDF<-matrix(0,5,2)%>%
  as.data.frame()
colnames(WolumenWeekDF)<-spolki

#korelogramy
ACFOrangeWeekWolumen<-acf(TygodniowyWolumen$Orange,plot=F)
ACFOrangeWeekWolumendf<-with(ACFOrangeWeekWolumen,data.frame(lag,acf))

ggplot(ACFOrangeWeekWolumendf,aes(lag,acf))+
  geom_bar(stat="identity",position="identity")+
  ggtitle("Korelogram logarytmicznych tygodniowych stóp zwrotu dla spółki Orange")+
  theme(plot.title = element_text(hjust=0.5))

ACFGTCWeekWolumen<-acf(TygodniowyWolumen$GTC,plot=F)
ACFGTCWeekWolumendf<-with(ACFGTCWeekWolumen,data.frame(lag,acf))

ggplot(ACFGTCWeekWolumendf,aes(lag,acf))+
  geom_bar(stat="identity",position="identity")+
  ggtitle("Korelogram logarytmicznych tygodniowych stóp zwrotu dla spółki Orange")+
  theme(plot.title = element_text(hjust=0.5))

for(i in 1:5){
  WolumenWeekDF$Orange[i]<-Box.test(TygodniowyWolumen$Orange,type="Ljung-Box",lag=i)$p.value
  WolumenWeekDF$GTC[i]<-Box.test(TygodniowyWolumen$GTC,type="Ljung-Box",lag=i)$p.value
}
