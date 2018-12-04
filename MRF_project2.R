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
library(normtest)


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

#wykres Orange
ggplot(TygodnioweStopyZwrotu,aes(x=Data,y=Orange))+
  geom_line(size=1.1,color="#4271AE")+
  ggtitle("Wykres tygodniowych logarytmicznych stóp zwrotu dla spółki Orange")+
  theme_economist(dkpanel=T)+
  theme(plot.title = element_text(hjust=0.5))

#wykres GTC
ggplot(TygodnioweStopyZwrotu,aes(x=Data,y=GTC))+
  geom_line(color="#4271AE",size=1.1)+
  ggtitle("Wykres tygodniowych logarytmicznych stóp zwrotu dla spółki GTC")+
  theme_economist(dkpanel=T)+
  theme(plot.title = element_text(hjust=0.5))

#testy stacjonarności
adf.test(TygodnioweStopyZwrotu$Orange,alternative="stationary")
adf.test(TygodnioweStopyZwrotu$GTC,alternative="stationary")

#statystyki opisowe
TygodnioweStatystyki<-describe(TygodnioweStopyZwrotu%>%
                                 select(Orange:GTC))%>%
  select(-c('vars','n','mad','range','trimmed','se'))%>%
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
randtests::runs.test(TygodnioweStopyZwrotu$Orange)
randtests::runs.test(TygodnioweStopyZwrotu$GTC)

#tesy normalności
as.numeric(shapiro.test(TygodnioweStopyZwrotu$Orange)$p.value)
as.numeric(shapiro.test(TygodnioweStopyZwrotu$GTC)$p.value)

as.numeric(lillie.test(TygodnioweStopyZwrotu$Orange)$p.value)
as.numeric(lillie.test(TygodnioweStopyZwrotu$GTC)$p.value)

as.numeric(jb.norm.test(TygodnioweStopyZwrotu$Orange)$p.value)
as.numeric(jb.norm.test(TygodnioweStopyZwrotu$GTC)$p.value)

ggplot(LogWeek,aes(x=StopaZwrotu))+
  geom_density(aes(color=Spolka),size=1.1)+
  theme_economist(dkpanel = T)+
  ggtitle("Wykresy gęstości tygodniowych logarytmicznych stóp zwrotu")

ggplot(LogWeek,aes(sample=StopaZwrotu))+
  stat_qq(aes(color=Spolka))+
  theme_economist(dkpanel=T)+
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
  geom_line(colour="#4271AE",size=1.1)+
  theme_economist(dkpanel=T)+
  ggtitle("Wykres szeregu czasowego tygodniowego wolumenu spółki Orange")+
  theme(plot.title = element_text(hjust=0.5))+
  ylab("Wartości tygodniowego wolumenu")

ggplot(TygodniowyWolumen,aes(Data,GTC))+
  geom_line(colour="#4271AE",size=1.1)+
  theme_economist(dkpanel=T)+
  ggtitle("Wykres szeregu czasowego tygodniowego wolumenu spółki GTC")+
  theme(plot.title = element_text(hjust=0.5))+
  ylab("Wartości tygodniowego wolumenu")

#testy stacjonarności
adf.test(TygodniowyWolumen$Orange,alternative="stationary")
adf.test(TygodniowyWolumen$GTC,alternative = "stationary")

#wykresy pudełkowe wolumenów
ggplot(TygodniowyWolumen,aes(y=Orange))+
  geom_boxplot(fill="#4271AE",colour="#1F3552",alpha=0.7,
               outlier.colour = "#1F3552",outlier.shape = 22)+
  theme_economist(dkpanel=T)+
  ggtitle("Wykres pudełkowy tygodniowego wolumenu spółki Orange")+
  theme(plot.title=element_text(hjust=0.5))+
  scale_x_discrete("Orange")+
  ylab("Wartości tygodniowego wolumenu")

ggplot(TygodniowyWolumen,aes(y=GTC))+
  geom_boxplot(fill="#4271AE",colour="#1F3552",alpha=0.7,
               outlier.colour = "#1F3552",outlier.shape = 22)+
  theme_economist(dkpanel = T)+
  ggtitle("Wykres pudełkowy tygodniowego wolumenu spółki GTC")+
  theme(plot.title=element_text(hjust=0.5))+
  scale_x_discrete("GTC")+
  ylab("Wartości tygodniowego wolumenu")

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
  geom_bar(stat="identity",position="identity",fill="#4271AE",colour="#1F3552")+
  ggtitle("Korelogram logarytmicznych tygodniowych stóp zwrotu dla spółki Orange")+
  theme(plot.title = element_text(hjust=0.5))+
  theme_economist(dkpanel=T)+
  xlab("Opóźnienie")+
  ylab("Wartości funkcji ACF")

ACFGTCWeekWolumen<-acf(TygodniowyWolumen$GTC,plot=F)
ACFGTCWeekWolumendf<-with(ACFGTCWeekWolumen,data.frame(lag,acf))

ggplot(ACFGTCWeekWolumendf,aes(lag,acf))+
  geom_bar(stat="identity",position="identity",fill="#4271AE",colour="#1F3552")+
  ggtitle("Korelogram logarytmicznych tygodniowych stóp zwrotu dla spółki GTC")+
  theme(plot.title = element_text(hjust=0.5))+
  theme_economist(dkpanel = T)+
  xlab("Opóźnienia")+
  ylab("Wartości funkcji ACF")

for(i in 1:5){
  WolumenWeekDF$Orange[i]<-Box.test(TygodniowyWolumen$Orange,type="Ljung-Box",lag=i)$p.value
  WolumenWeekDF$GTC[i]<-Box.test(TygodniowyWolumen$GTC,type="Ljung-Box",lag=i)$p.value
}


# Analiza miesięcznych stóp zwrotu ----------------------------------------


# Analiza miesięcznych wolumenów obrotu -----------------------------------
MiesiecznyWolumen<-data.frame(OrangeMonth$Data,OrangeMonth$Wolumen,
                              GTCMonth$Wolumen)
colnames(MiesiecznyWolumen)<-c("Data",spolki)
MiesiecznyWolumen$Data<-as.Date(MiesiecznyWolumen$Data)

#wykresy szeregów czasowych
ggplot(MiesiecznyWolumen,aes(Data,Orange))+
  geom_line(colour="#4271AE",size=1.1)+
  theme_economist(dkpanel=T)+
  ggtitle("Wykres szeregu czasowego miesięcznego wolumenu spółki GTC")+
  theme(plot.title = element_text(hjust=0.5))+
  ylab("Wartości miesięczneego wolumenu")

ggplot(MiesiecznyWolumen,aes(Data,GTC))+
  geom_line(colour="#4271AE",size=1.1)+
  theme_economist(dkpanel=T)+
  ggtitle("Wykres szeregu czasowego miesięcznego wolumenu spółki GTC")+
  theme(plot.title = element_text(hjust=0.5))+
  ylab("Wartości miesięcznego wolumenu")

#wykresy pudełkowe
ggplot(MiesiecznyWolumen,aes(y=Orange))+
  geom_boxplot(fill="#4271AE",colour="#1F3552",alpha=0.7,
               outlier.colour = "#1F3552",outlier.shape = 22)+
  theme_economist(dkpanel = T)+
  ggtitle("Wykres pudełkowy miesięcznego wolumenu spółki Orange")+
  theme(plot.title=element_text(hjust=0.5))+
  scale_x_discrete("Orange")+
  ylab("Wartości miesięcznego wolumenu")

ggplot(MiesiecznyWolumen,aes(y=GTC))+
  geom_boxplot(fill="#4271AE",colour="#1F3552",alpha=0.7,
               outlier.colour = "#1F3552",outlier.shape = 22)+
  theme_economist(dkpanel = T)+
  ggtitle("Wykres pudełkowy tygodniowego wolumenu spółki GTC")+
  theme(plot.title=element_text(hjust=0.5))+
  scale_x_discrete("GTC")+
  ylab("Wartości miesięcznego wolumenu")

#testy zgodności z rozkładem normalnym oraz log-normalnym
ks.test(MiesiecznyWolumen$Orange,"pnorm")
ks.test(MiesiecznyWolumen$GTC,"pnorm")

ks.test(MiesiecznyWolumen$Orange,"plnorm")
ks.test(MiesiecznyWolumen$GTC,"plnorm")

#badanie autokorelacji
WolumenMonthDF<-matrix(0,5,2)%>%
  as.data.frame()
colnames(WolumenMonthDF)<-spolki

#korelogramy
ACFOrangeMonthWolumen<-acf(MiesiecznyWolumen$Orange,plot=F)
ACFOrangeMonthWolumendf<-with(ACFOrangeMonthWolumen,data.frame(lag,acf))

ggplot(ACFOrangeMonthWolumendf,aes(lag,acf))+
  geom_bar(stat="identity",position="identity",fill="#4271AE",color="#1F3552")+
  ggtitle("Korelogram miesięcznego wolumenu spółki Orange")+
  theme_economist(dkpanel = T)+
  theme(plot.title = element_text(hjust=0.5))+
  xlab("Opóźnienie")+
  ylab("Wartości funkcji ACF")

ACFGTCMonthWolumen<-acf(MiesiecznyWolumen$GTC,plot=F)
ACFGTCMonthWolumendf<-with(ACFGTCMonthWolumen,data.frame(lag,acf))

ggplot(ACFGTCMonthWolumendf,aes(lag,acf))+
  geom_bar(stat="identity",position="identity",fill="#4271AE",color="#1F3552")+
  ggtitle("Korelogram miesięcznego wolumenu spółki GTC")+
  theme_economist(dkpanel=T)+
  theme(plot.title = element_text(hjust=0.5))+
  xlab("Opóźnienie")+
  ylab("Wartości funkcji ACF")

for(i in 1:5){
  WolumenMonthDF$Orange[i]<-Box.test(MiesiecznyWolumen$Orange,type="Ljung-Box",lag=i)$p.value
  WolumenMonthDF$GTC[i]<-Box.test(MiesiecznyWolumen$GTC,type="Ljung-Box",lag=i)$p.value
}
