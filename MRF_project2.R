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
library(ggfortify)

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

alpha<-0.95 #poziom przedziału ufności

# Analiza tygodniowych stóp zwrotu ----------------------------------------

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
  t()%>%
  round(digit=2)

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

#wykresy gęstości
ggplot(LogWeek,aes(x=StopaZwrotu))+
  geom_density(aes(color=Spolka),size=1.1)+
  theme_economist(dkpanel = T)+
  ggtitle("Wykresy gęstości tygodniowych logarytmicznych stóp zwrotu")

#wykresy kwantyl-kwantyl
ggplot(LogWeek,aes(sample=StopaZwrotu))+
  stat_qq(aes(color=Spolka))+
  theme_economist(dkpanel=T)+
  ggtitle("Wykresy kwantyl-kwantyl tygodniowych logarytmicznych stóp zwrotu")

#testy autokorelacji
ACFOrangeWeek<-acf(TygodnioweStopyZwrotu$Orange,plot=F)
ACFOrangeWeekdf<-with(ACFOrangeWeek,data.frame(lag,acf))
OrangeWeekCI<-c(-1,1)*qnorm((1+alpha)/2)/sqrt(ACFOrangeWeek$n.used)

ACFGTCWeek<-acf(TygodnioweStopyZwrotu$GTC,plot=F)
ACFGTCWeekdf<-with(ACFGTCWeek,data.frame(lag,acf))
GTCWeekCI<-c(-1,1)*qnorm((1+alpha)/2)/sqrt(ACFGTCWeek$n.used)

ggplot(ACFOrangeWeekdf,aes(lag,acf))+
  geom_bar(stat="identity",position="identity",fill="#4271AE",colour="#1F3552")+
  ggtitle("Korelogram logarytmicznych tygodniowych stóp zwrotu dla spółki Orange")+
  theme_economist(dkpanel=T)+
  theme(plot.title = element_text(hjust=0.5))+
  xlab("Opóźnienie")+
  ylab("Wartość funkcji ACF")+
  scale_x_continuous(breaks=seq(0,25,by=5))+
  geom_hline(yintercept=OrangeWeekCI, lty=2, col='red')

ggplot(ACFGTCWeekdf,aes(lag,acf))+
  geom_bar(stat="identity",position="identity",fill="#4271AE",colour="#1F3552")+
  ggtitle("Korelogram logarytmicznych tygodniowych stóp zwrotu dla spółki GTC")+
  theme_economist(dkpanel=T)+
  theme(plot.title = element_text(hjust=0.5))+
  xlab("Opóźnienie")+
  ylab("Wartość funkcji ACF")+
  scale_x_continuous(breaks=seq(0,25,by=5))+
  geom_hline(yintercept=GTCWeekCI, lty=2, col='red')

#test Ljung-Boxa
LBResultWeek<-matrix(0,5,2)%>%
  as.data.frame()
colnames(LBResultWeek)<-spolki

for(i in 1:5){
  LBResultWeek$Orange[i]<-Box.test(TygodnioweStopyZwrotu$Orange,type="Ljung-Box",lag = i)$p.value
  LBResultWeek$GTC[i]<-Box.test(TygodnioweStopyZwrotu$GTC,type="Ljung-Box",lag = i)$p.value
}

round(LBResultWeek,digit=2)

# Analiza tygodniowych wolumenów obrotu -----------------------------------

TygodniowyWolumen<-data.frame(OrangeWeek$Data,OrangeWeek$Wolumen,GTCWeek$Wolumen)
colnames(TygodniowyWolumen)<-c("Data",spolki)
TygodniowyWolumen$Data<-as.Date(TygodniowyWolumen$Data)

#wykresy szeregów czasowych
#wykres Orange
ggplot(TygodniowyWolumen,aes(Data,Orange))+
  geom_line(colour="#4271AE",size=1.1)+
  theme_economist(dkpanel=T)+
  ggtitle("Wykres szeregu czasowego tygodniowego wolumenu spółki Orange")+
  theme(plot.title = element_text(hjust=0.5))+
  ylab("Wartości tygodniowego wolumenu")

#wykres GTC
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
WolumenWeek<-TygodniowyWolumen%>%
  gather(Spolka,Wolumen,Orange:GTC)

ggplot(WolumenWeek,aes(x=as.factor(Spolka),y=Wolumen))+
  geom_boxplot(fill="#4271AE",colour="#1F3552",alpha=0.7,
               outlier.colour = "#1F3552",outlier.shape = 22)+
  scale_x_discrete(name="Spółka")+
  ggtitle("Wykresy pudełkowe tygodniowych wolumenów obrotu")+
  theme_economist(dkpanel=T)+
  theme(plot.title = element_text(hjust=0.5))

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
OrangeWeekWolumenCI<-c(-1,1)*qnorm((1+alpha)/2)/sqrt(ACFOrangeWeekWolumen$n.used)

ggplot(ACFOrangeWeekWolumendf,aes(lag,acf))+
  geom_bar(stat="identity",position="identity",fill="#4271AE",colour="#1F3552")+
  ggtitle("Korelogram tygodniowego wolumenu obrotu dla spółki Orange")+
  theme(plot.title = element_text(hjust=0.5))+
  theme_economist(dkpanel=T)+
  xlab("Opóźnienie")+
  ylab("Wartości funkcji ACF")+
  geom_hline(yintercept=OrangeWeekWolumenCI, lty=2, col='red')

ACFGTCWeekWolumen<-acf(TygodniowyWolumen$GTC,plot=F)
ACFGTCWeekWolumendf<-with(ACFGTCWeekWolumen,data.frame(lag,acf))
GTCWeekWolumenCI<-c(-1,1)*qnorm((1+alpha)/2)/sqrt(ACFGTCWeekWolumen$n.used)

ggplot(ACFGTCWeekWolumendf,aes(lag,acf))+
  geom_bar(stat="identity",position="identity",fill="#4271AE",colour="#1F3552")+
  ggtitle("Korelogram tygodniowego wolumenu obrotu dla spółki GTC")+
  theme(plot.title = element_text(hjust=0.5))+
  theme_economist(dkpanel = T)+
  xlab("Opóźnienia")+
  ylab("Wartości funkcji ACF")+
  geom_hline(yintercept=GTCWeekWolumenCI, lty=2, col='red')

#test Ljung-Boxa
for(i in 1:5){
  WolumenWeekDF$Orange[i]<-Box.test(TygodniowyWolumen$Orange,type="Ljung-Box",lag=i)$p.value
  WolumenWeekDF$GTC[i]<-Box.test(TygodniowyWolumen$GTC,type="Ljung-Box",lag=i)$p.value
}

# Analiza miesięcznych stóp zwrotu ----------------------------------------

MiesieczneStopyZwrotu<-matrix(0,nrow(OrangeMonth)-1,3)%>%
  as.data.frame()
colnames(MiesieczneStopyZwrotu)<-c("Data",spolki)

MiesieczneStopyZwrotu$Orange<-log((OrangeMonth$Zamkniecie[2:nrow(OrangeMonth)]/
                                     OrangeMonth$Zamkniecie[1:(nrow(OrangeMonth)-1)]))
MiesieczneStopyZwrotu$GTC<-log((GTCMonth$Zamkniecie[2:nrow(GTCMonth)]/
                                  GTCMonth$Zamkniecie[1:(nrow(GTCMonth)-1)]))
MiesieczneStopyZwrotu$Data<-as.Date(OrangeMonth$Data[2:nrow(OrangeMonth)])

#wykresy szeregów czasowych
LogMonth<-MiesieczneStopyZwrotu%>%
  gather(Spolka,StopaZwrotu,Orange:GTC)

#wykres Orange
ggplot(MiesieczneStopyZwrotu,aes(x=Data,y=Orange))+
  geom_line(size=1.1,color="#4271AE")+
  ggtitle("Wykres miesięcznych logarytmicznych stóp zwrotu dla spółki Orange")+
  theme_economist(dkpanel=T)+
  theme(plot.title = element_text(hjust=0.5))

#wykres GTC
ggplot(MiesieczneStopyZwrotu,aes(x=Data,y=GTC))+
  geom_line(color="#4271AE",size=1.1)+
  ggtitle("Wykres miesięcznych logarytmicznych stóp zwrotu dla spółki GTC")+
  theme_economist(dkpanel=T)+
  theme(plot.title = element_text(hjust=0.5))

#testy stacjonarności
adf.test(MiesieczneStopyZwrotu$Orange,alternative="stationary")
adf.test(MiesieczneStopyZwrotu$GTC,alternative="stationary")

#statystyki opisowe
MiesieczneeStatystyki<-describe(MiesieczneStopyZwrotu%>%
                                 select(Orange:GTC))%>%
  select(-c('vars','n','mad','range','trimmed','se'))%>%
  t()%>%
  round(digit=2)

#wykresy pudełkowe
ggplot(LogMonth,aes(x=as.factor(Spolka),y=StopaZwrotu))+
  geom_boxplot(fill="#4271AE",colour="#1F3552",alpha=0.7,
               outlier.colour = "#1F3552",outlier.shape = 22)+
  scale_x_discrete(name="Spółka")+
  ggtitle("Wykresy pudełkowe miesięcznych logarytmicznych stóp zwrotu")+
  theme_economist(dkpanel=T)+
  theme(plot.title = element_text(hjust=0.5))

#test Walda-Wolfowitza na losowość
randtests::runs.test(MiesieczneStopyZwrotu$Orange)
randtests::runs.test(MiesieczneStopyZwrotu$GTC)

#tesy normalności
as.numeric(shapiro.test(MiesieczneStopyZwrotu$Orange)$p.value)
as.numeric(shapiro.test(MiesieczneStopyZwrotu$GTC)$p.value)

as.numeric(lillie.test(MiesieczneStopyZwrotu$Orange)$p.value)
as.numeric(lillie.test(MiesieczneStopyZwrotu$GTC)$p.value)

as.numeric(jb.norm.test(MiesieczneStopyZwrotu$Orange)$p.value)
as.numeric(jb.norm.test(MiesieczneStopyZwrotu$GTC)$p.value)

#wykresy gęstości
ggplot(LogMonth,aes(x=StopaZwrotu))+
  geom_density(aes(color=Spolka),size=1.1)+
  theme_economist(dkpanel = T)+
  ggtitle("Wykresy gęstości miesięcznych logarytmicznych stóp zwrotu")

#wykresy kwantyl-kwantyl
ggplot(LogWeek,aes(sample=StopaZwrotu))+
  stat_qq(aes(color=Spolka))+
  theme_economist(dkpanel=T)+
  ggtitle("Wykresy kwantyl-kwantyl miesięcznych logarytmicznych stóp zwrotu")

#testy autokorelacji
ACFOrangeMonth<-acf(MiesieczneStopyZwrotu$Orange,plot=F)
ACFOrangeMonthdf<-with(ACFOrangeMonth,data.frame(lag,acf))
OrangeMonthCI<-c(-1,1)*qnorm((1+alpha)/2)/sqrt(ACFOrangeMonth$n.used)

ACFGTCMonth<-acf(MiesieczneStopyZwrotu$GTC,plot=F)
ACFGTCMonthdf<-with(ACFGTCMonth,data.frame(lag,acf))
GTCMonthCI<-c(-1,1)*qnorm((1+alpha)/2)/sqrt(ACFGTCMonth$n.used)

ggplot(ACFOrangeMonthdf,aes(lag,acf))+
  geom_bar(stat="identity",position="identity",fill="#4271AE",colour="#1F3552")+
  ggtitle("Korelogram logarytmicznych miesięcznych stóp zwrotu dla spółki Orange")+
  theme_economist(dkpanel=T)+
  theme(plot.title = element_text(hjust=0.5))+
  xlab("Opóźnienie")+
  ylab("Wartość funkcji ACF")+
  scale_x_continuous(breaks=seq(0,25,by=5))+
  geom_hline(yintercept=OrangeMonthCI, lty=2, col='red')

ggplot(ACFGTCMonthdf,aes(lag,acf))+
  geom_bar(stat="identity",position="identity",fill="#4271AE",colour="#1F3552")+
  ggtitle("Korelogram logarytmicznych miesięcznych stóp zwrotu dla spółki GTC")+
  theme_economist(dkpanel=T)+
  theme(plot.title = element_text(hjust=0.5))+
  xlab("Opóźnienie")+
  ylab("Wartość funkcji ACF")+
  scale_x_continuous(breaks=seq(0,25,by=5))+
  geom_hline(yintercept=GTCMonthCI, lty=2, col='red')

#test Ljung-Boxa
LBResultMonth<-matrix(0,5,2)%>%
  as.data.frame()
colnames(LBResultMonth)<-spolki

for(i in 1:5){
  LBResultMonth$Orange[i]<-Box.test(MiesieczneStopyZwrotu$Orange,type="Ljung-Box",lag = i)$p.value
  LBResultMonth$GTC[i]<-Box.test(MiesieczneStopyZwrotu$GTC,type="Ljung-Box",lag = i)$p.value
}

round(LBResultMonth,digit=2)

# Analiza miesięcznych wolumenów obrotu -----------------------------------

MiesiecznyWolumen<-data.frame(OrangeMonth$Data,OrangeMonth$Wolumen,
                              GTCMonth$Wolumen)
colnames(MiesiecznyWolumen)<-c("Data",spolki)
MiesiecznyWolumen$Data<-as.Date(MiesiecznyWolumen$Data)

#wykresy szeregów czasowych
#wykres Orange
ggplot(MiesiecznyWolumen,aes(Data,Orange))+
  geom_line(colour="#4271AE",size=1.1)+
  theme_economist(dkpanel=T)+
  ggtitle("Wykres szeregu czasowego miesięcznego wolumenu spółki Orange")+
  theme(plot.title = element_text(hjust=0.5))+
  ylab("Wartości miesięczneego wolumenu")

#wykres GTC
ggplot(MiesiecznyWolumen,aes(Data,GTC))+
  geom_line(colour="#4271AE",size=1.1)+
  theme_economist(dkpanel=T)+
  ggtitle("Wykres szeregu czasowego miesięcznego wolumenu spółki GTC")+
  theme(plot.title = element_text(hjust=0.5))+
  ylab("Wartości miesięcznego wolumenu")

#testy stacjonarności
adf.test(MiesiecznyWolumen$Orange,alternative="stationary")
adf.test(MiesiecznyWolumen$GTC,alternative = "stationary")

#wykresy pudełkowe
WolumenMonth<-MiesiecznyWolumen%>%
  gather(Spolka,Wolumen,Orange:GTC)

ggplot(WolumenMonth,aes(x=as.factor(Spolka),y=Wolumen))+
  geom_boxplot(fill="#4271AE",colour="#1F3552",alpha=0.7,
               outlier.colour = "#1F3552",outlier.shape = 22)+
  scale_x_discrete(name="Spółka")+
  ggtitle("Wykresy pudełkowe tygodniowych wolumenów obrotu")+
  theme_economist(dkpanel=T)+
  theme(plot.title = element_text(hjust=0.5))

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
OrangeMonthWolumenCI<-c(-1,1)*qnorm((1 + alpha)/2)/sqrt(ACFOrangeMonthWolumen$n.used)

ggplot(ACFOrangeMonthWolumendf,aes(lag,acf))+
  geom_bar(stat="identity",position="identity",fill="#4271AE",color="#1F3552")+
  ggtitle("Korelogram miesięcznego wolumenu obrotu dla spółki Orange")+
  theme_economist(dkpanel = T)+
  theme(plot.title = element_text(hjust=0.5))+
  xlab("Opóźnienie")+
  ylab("Wartości funkcji ACF")+
  geom_hline(yintercept=OrangeMonthWolumenCI, lty=2, col='red')

ACFGTCMonthWolumen<-acf(MiesiecznyWolumen$GTC,plot=F)
ACFGTCMonthWolumendf<-with(ACFGTCMonthWolumen,data.frame(lag,acf))
GTCMonthWolumenCI<-c(-1,1)*qnorm((1+alpha)/2)/sqrt(ACFGTCMonthWolumen$n.used)

ggplot(ACFGTCMonthWolumendf,aes(lag,acf))+
  geom_bar(stat="identity",position="identity",fill="#4271AE",color="#1F3552")+
  ggtitle("Korelogram miesięcznego wolumenu obrotu dla spółki GTC")+
  theme_economist(dkpanel=T)+
  theme(plot.title = element_text(hjust=0.5))+
  xlab("Opóźnienie")+
  ylab("Wartości funkcji ACF")+
  geom_hline(yintercept=GTCMonthWolumenCI, lty=2, col='red')

#test Ljung-Boxa
for(i in 1:5){
  WolumenMonthDF$Orange[i]<-Box.test(MiesiecznyWolumen$Orange,type="Ljung-Box",lag=i)$p.value
  WolumenMonthDF$GTC[i]<-Box.test(MiesiecznyWolumen$GTC,type="Ljung-Box",lag=i)$p.value
}
