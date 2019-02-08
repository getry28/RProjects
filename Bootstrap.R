#Bootstrap project
#Created by: Patryk Zieliński, WZ AGH
#Kraków 2018

library(tidyr)
library(dplyr)
library(ggplot2)
library(psych)
library(ggcorrplot)
library(ggthemes)
library(normtest)
library(nortest)
library(randtests)
library(lmtest)
library(car)
library(strucchange)
library(simpleboot)
library(gridExtra)

hellwig <- function(y, x, method="pearson")
{
  requireNamespace("utils")
  x <- as.data.frame(x)
  cm <- stats::cor(x, method=method) # correlation matrix among indeps
  cd <- stats::cor(x, y, method=method) # correlations with dependent
  # list of combination vectors
  k <- sapply( seq(2, length(x)), function(i)
    utils::combn(length(x), i, simplify=FALSE) )
  k <- do.call("c", k)
  # function calculating individual capacities
  hfun <- function(v)
  {
    sapply(v, function(i) cd[i]^2 / sum(abs(cm[v,i])) )
  }
  h <- sapply(k, hfun)
  data.frame( k = sapply( k, paste, collapse="-"),
              h = sapply(h, sum),
              stringsAsFactors=FALSE)
}

# Import danych -------------------------------------------------------------

#wczytanie pliku
CarsCSV<-read.csv("https://raw.githubusercontent.com/getry28/RProjects/master/cars.csv",
                  header=T)

#konwersja danych
USGallon<- 3.785411784
Mile<-1.609344
Cal3<-0.02
Funt<-0.45359237
Kmh<-1.609344

CarsPL<-CarsCSV%>%
  mutate(Spalanie = 100*USGallon/(Mile*mpg))%>%
  mutate(ObjetoscSilnika = Cal3*displacement)%>%
  mutate(Waga = weight*Funt)%>%
  mutate(CzasPrzyspieszenia = acceleration*100/(60*Kmh))%>%
  mutate(WiekSamochodu = 2019 - (year+1900))%>%
  select(-c('mpg','displacement','weight','acceleration','year'))

CarsPL$Waga<-round(CarsPL$Waga,digits = 0)
CarsPL$CzasPrzyspieszenia<-round(CarsPL$CzasPrzyspieszenia,digits=1)
CarsPL$Spalanie<-round(CarsPL$Spalanie,digits=1)

Kolumny<-c("LiczbaCylindrow","Moc","MiejsceProdukcji",
           "NazwaSamochodu","Spalanie","ObjetoscSilnika",
           "Waga","CzasPrzyspieszenia","WiekSamochodu")
colnames(CarsPL)<-Kolumny

# Tworzenie statystyk opisowych -----------------------------------------

#tabela statystyk opisowych
CarsDS<-describe(CarsPL%>%
                   select(-c('NazwaSamochodu','MiejsceProdukcji')))%>%
  select(-c('vars','n','trimmed','mad','range','se'))%>%
  mutate(WspolczynnikZmiennosci = sd/mean)%>%
  round(digits=3) 

rownames(CarsDS)<-colnames(CarsPL %>%
                             select(-c('NazwaSamochodu','MiejsceProdukcji')))

colnames(CarsDS)<-c("Srednia","Odchylenie Standardowe","Mediana",
                       "Minimum","Maksimum","Skośność","Kurtoza",
                       "Współczynnik zmienności")

#uporządkowanie ramki
col_order<-c("Spalanie","NazwaSamochodu","LiczbaCylindrow","Moc","WiekSamochodu",
             "MiejsceProdukcji",
             "ObjetoscSilnika","Waga","CzasPrzyspieszenia")

CarsPL<-CarsPL[,col_order]

CarsPL$MiejsceProdukcjiT[CarsPL$MiejsceProdukcji=="1"]<-"USA"
CarsPL$MiejsceProdukcjiT[CarsPL$MiejsceProdukcji=="2"]<-"Europa"
CarsPL$MiejsceProdukcjiT[CarsPL$MiejsceProdukcji=="3"]<-"Japonia"

LiczbaDanych<-NROW(CarsPL)

# Analiza wizualna danych -----------------------------------

#macierz korelacji
MacierzKorelacji<-cor(CarsPL%>%
                        select(-c('NazwaSamochodu','MiejsceProdukcjiT')))

#wykres macierzy korelacji
p.mat<-cor_pmat(MacierzKorelacji)

ggcorrplot(MacierzKorelacji,type="upper",outline.col="white",p.mat=p.mat)+
  ggtitle("Wyniki testu istotności współczynnika korelacji")+
  theme(plot.title=element_text(hjust=0.5))

ggcorrplot(MacierzKorelacji,type="upper",outline.col="white",lab=T)+
  ggtitle("Macierz korelacji zmiennych")+
  theme(plot.title=element_text(hjust=0.5))

#do usunięcia: waga i liczba cylindrów

#wykresy

#nr 1 - histogram do czasu przyspieszenia
ggplot(CarsPL,aes(CzasPrzyspieszenia))+
  geom_histogram(binwidth = 1,aes(fill=..count..))+
  theme_gdocs()+
  ylab("Liczność")+
  xlab("Czas przyspieszenia samochodu do 100 km/h w sekundach")+
  ggtitle("Histogram czasu przyspieszenia samochodów")+
  theme(plot.title=element_text(hjust=0.5))+
  scale_fill_gradient("Liczność",low="blue",high="red")

#nr 2 - sprawdzenie czy rok produkcji ma wpływ na moc silnika - histogram
CarsPL$MocSrednia<-ifelse(CarsPL$Moc>mean(CarsPL$Moc),"powyżej","poniżej")
CarsPL$RokProdukcji<- 2019-CarsPL$WiekSamochodu

ggplot(CarsPL,aes(x=RokProdukcji,fill=MocSrednia))+
  geom_histogram(position="dodge",binwidth=0.5)+
  theme_gdocs()+
  ggtitle("Rozkład mocy silnika względem roku produkcji")+
  xlab("Rok produkcji samochodu")+
  ylab("Liczność")+
  labs(fill="Moc silnika")+
  scale_x_continuous(breaks=seq(1970,1982,by=2))
  
#nr 3 - szereg czasowy dla roku produkcji z podziałem na miejsce produkcji
CarsPLSzereg<-CarsPL%>%
  group_by(RokProdukcji, MiejsceProdukcjiT)%>%
  summarise(LiczbaSamochodow=n())

ggplot(CarsPLSzereg,aes(RokProdukcji, LiczbaSamochodow))+
  geom_line(aes(color=MiejsceProdukcjiT))+
  theme_gdocs()+
  ggtitle("Wykres liczby wyprodukowanych samochodów w każdym z badanych miejsc")+
  xlab("Rok produkcji samochodu")+
  ylab("Liczba wyprodukowanych samochodów")+
  labs(color="Miejsce produkcji")+
  scale_x_continuous(breaks=seq(1970,1982,by=2))

#nr 4 - scatterplot --> spalanie od objętości silnika + miejsce produkcji jako kategoria
ggplot(CarsPL,aes(ObjetoscSilnika,Spalanie,
                  color=as.factor(MiejsceProdukcjiT)))+
  geom_jitter()+
  theme_gdocs()+
  ggtitle("Wykres spalania względem objętości silnika")+
  theme(plot.title=element_text(hjust=0.5))+
  xlab("Objętość silnika w litrach")+
  ylab("Spalanie w l/100km")+
  labs(color="Miejsce produkcji")

# Regresja liniowa ------------------------------------------------
RegresjaLiniowa<-lm(Spalanie~Moc+WiekSamochodu+MiejsceProdukcji+ObjetoscSilnika+
                CzasPrzyspieszenia,CarsPL)

#diagnostyka modelu

#przedziały ufności dla parametrów
RegresjaLiniowaCI<-confint(RegresjaLiniowa, level=0.95)

#błędy szacunku
sqrt(diag(vcov(RegresjaLiniowa)))

#diagnostyka reszt
ResztyRegresjiLiniowej<-RegresjaLiniowa$residuals

ggplot(RegresjaLiniowa,aes(sample=ResztyRegresjiLiniowej))+
  theme_gdocs()+
  stat_qq()+
  stat_qq_line()+
  ggtitle("Wykres Q-Q reszt modelu regresji liniowej")+
  theme(plot.title=element_text(hjust=0.5))+
  xlab("Kwantyle teoretyczne")+
  ylab("Kwantyle z próby")

ggplot(data=RegresjaLiniowa,aes(ResztyRegresjiLiniowej))+
  geom_histogram(binwidth = 0.5, aes(y=..density.., fill=..count..))+
  theme_gdocs()+
  stat_function(fun=dnorm,color="forestgreen",size=1.25,
                args=list(mean=mean(ResztyRegresjiLiniowej), 
                          sd=sd(ResztyRegresjiLiniowej)))+
  ggtitle("Rozkład reszt modelu regresji liniowej")+
  theme(plot.title=element_text(hjust=0.5))+
  xlab("Reszty regresji liniowej")+
  ylab("Gęstość")+
  scale_fill_gradient("Liczność",low="blue",high="red")

jb.norm.test(ResztyRegresjiLiniowej)
lillie.test(ResztyRegresjiLiniowej)
shapiro.test(ResztyRegresjiLiniowej)

#reszty nie mają rozkładu normalnego - brak dalszej możliwości
#przeprowadzenia diagnostyki modelu

# Regresja liniowa na podstawie metody Hellwiga -------------------------

#Hellwig method
Hellwig<-hellwig(CarsPL$Spalanie,CarsPL %>%
                   select(Moc:ObjetoscSilnika,CzasPrzyspieszenia),method="pearson")%>%
  arrange(desc(h))

zmienne<-Hellwig$k[1]%>%
  strsplit("-")%>%
  unlist()%>%
  as.numeric()

#wyszło: Moc, WiekSamochodu, ObjetoscSilnika

#model skonstruowany za pomocą metody Hellwiga
RegresjaHellwig<-lm(Spalanie~Moc+WiekSamochodu+ObjetoscSilnika,CarsPL)

#diagnostyka modelu

#przedziały ufności dla parametrów
RegresjaHellwigCI<-confint(RegresjaHellwig, level=0.95)

#błędy szacunku
sqrt(diag(vcov(RegresjaHellwig)))

#analiza reszt
ResztyHellwiga<-RegresjaHellwig$residuals

ggplot(RegresjaHellwig,aes(sample=ResztyHellwiga))+
  theme_gdocs()+
  stat_qq()+
  stat_qq_line()+
  ggtitle("Wykres Q-Q reszt modelu regresji liniowej
          \nz wykorzystaniem metody Hellwiga")+
  theme(plot.title=element_text(hjust=0.5))+
  xlab("Kwantyle teoretyczne")+
  ylab("Kwantyle z próby")

ggplot(data=RegresjaHellwig,aes(ResztyHellwiga))+
  theme_gdocs()+
  geom_histogram(binwidth = 0.5, aes(y=..density.., fill=..count..))+
  stat_function(fun=dnorm,color="forestgreen",size=1.25,
                args=list(mean=mean(ResztyHellwiga), 
                          sd=sd(ResztyHellwiga)))+
  ggtitle("Rozkład reszt modelu regresji liniowej z wykorzystaniem metody Hellwiga")+
  theme(plot.title=element_text(hjust=0.5))+
  xlab("Reszty modelu regresji")+
  ylab("Gęstość")+
  scale_fill_gradient("Liczność",low="blue",high="red")

jb.norm.test(ResztyHellwiga)
lillie.test(ResztyHellwiga)
shapiro.test(ResztyHellwiga)

#reszty nie mają rozkładu normalnego



# Bootstrap do regresji liniowej ---------------------------------------------------------------

set.seed(1000)
RegresjaB1 <- lm.boot(RegresjaLiniowa, R = 1000)
summary(RegresjaB1)

#próba bootstrapowa B1
B1WS<-as.data.frame(t(samples(RegresjaB1,"coef")))

set.seed(1000)
RegresjaB2<- lm.boot(RegresjaLiniowa,R=1000, rows=FALSE)
summary(RegresjaB2)

#próba bootstrapowa B2
B2WS<-as.data.frame(t(samples(RegresjaB2,"coef")))


# Próba bootstrapowa B1 ---------------------------------------------------
KwantyleB1<-as.data.frame(matrix(nrow=6,ncol=2,0))
colnames(KwantyleB1)<-c("2,5%","97,5%")
rownames(KwantyleB1)<-colnames(B1WS)

for(i in 1:6){
  KwantyleB1[i,1]<-quantile(B1WS[,i],probs=.025)
  KwantyleB1[i,2]<-quantile(B1WS[,i],probs=.975)
}

B11<-ggplot(B1WS,aes(B1WS$`(Intercept)`))+
  geom_histogram(binwidth = 1,aes(fill=..count..))+
  geom_vline(data=B1WS,aes(xintercept = KwantyleB1[1,1]),color="forestgreen")+
  geom_vline(data=B1WS,aes(xintercept = KwantyleB1[1,2]),color="forestgreen")+
  theme_gdocs()+
  ylab("Liczność")+
  xlab("Wartości stałej")+
  ggtitle("Rozkład próby bootstrapowej dla stałej")+
  theme(plot.title=element_text(hjust=0.5))+
  scale_fill_gradient("Liczność",low="blue",high="red")

B12<-ggplot(B1WS,aes(B1WS$Moc))+
  geom_histogram(binwidth = 0.004,aes(fill=..count..))+
  geom_vline(data=B1WS,aes(xintercept = KwantyleB1[2,1]),color="forestgreen")+
  geom_vline(data=B1WS,aes(xintercept = KwantyleB1[2,2]),color="forestgreen")+
  theme_gdocs()+
  ylab("Liczność")+
  xlab("Wartości współczynnika")+
  ggtitle("Rozkład próby bootstrapowej\n dla zmiennej Moc")+
  theme(plot.title=element_text(hjust=0.5))+
  scale_fill_gradient("Liczność",low="blue",high="red")

B13<-ggplot(B1WS,aes(B1WS$WiekSamochodu))+
  geom_histogram(binwidth = 0.01,aes(fill=..count..))+
  geom_vline(data=B1WS,aes(xintercept = KwantyleB1[3,1]),color="forestgreen")+
  geom_vline(data=B1WS,aes(xintercept = KwantyleB1[3,2]),color="forestgreen")+
  theme_gdocs()+
  ylab("Liczność")+
  xlab("Wartości współczynnika")+
  ggtitle("Rozkład próby bootstrapowej\n dla zmiennej WiekSamochodu")+
  theme(plot.title=element_text(hjust=0.5))+
  scale_fill_gradient("Liczność",low="blue",high="red")

B14<-ggplot(B1WS,aes(B1WS$MiejsceProdukcji))+
  geom_histogram(binwidth = 0.05,aes(fill=..count..))+
  geom_vline(data=B1WS,aes(xintercept = KwantyleB1[4,1]),color="forestgreen")+
  geom_vline(data=B1WS,aes(xintercept = KwantyleB1[4,2]),color="forestgreen")+
  theme_gdocs()+
  ylab("Liczność")+
  xlab("Wartości współczynnika")+
  ggtitle("Rozkład próby bootstrapowej\n dla zmiennej MiejsceProdukcji")+
  theme(plot.title=element_text(hjust=0.5))+
  scale_fill_gradient("Liczność",low="blue",high="red")

B15<-ggplot(B1WS,aes(B1WS$ObjetoscSilnika))+
  geom_histogram(binwidth = 0.1,aes(fill=..count..))+
  geom_vline(data=B1WS,aes(xintercept = KwantyleB1[5,1]),color="forestgreen")+
  geom_vline(data=B1WS,aes(xintercept = KwantyleB1[5,2]),color="forestgreen")+
  theme_gdocs()+
  ylab("Liczność")+
  xlab("Wartości współczynnika")+
  ggtitle("Rozkład próby bootstrapowej\n dla zmiennej ObjetoscSilnika")+
  theme(plot.title=element_text(hjust=0.5))+
  scale_fill_gradient("Liczność",low="blue",high="red")

B16<-ggplot(B1WS,aes(B1WS$CzasPrzyspieszenia))+
  geom_histogram(binwidth = 0.02,aes(fill=..count..))+
  geom_vline(data=B1WS,aes(xintercept = KwantyleB1[6,1]),color="forestgreen")+
  geom_vline(data=B1WS,aes(xintercept = KwantyleB1[6,2]),color="forestgreen")+
  theme_gdocs()+
  ylab("Liczność")+
  xlab("Wartości współczynnika")+
  ggtitle("Rozkład próby bootstrapowej\n dla zmiennej CzasPrzyspieszenia")+
  theme(plot.title=element_text(hjust=0.5))+
  scale_fill_gradient("Liczność",low="blue",high="red")

grid.arrange(B11,B12,B13,B14,B15,B16, ncol = 2, nrow = 3)


# Próba bootstrapowa B2 ---------------------------------------------------

KwantyleB2<-as.data.frame(matrix(nrow=6,ncol=2,0))
colnames(KwantyleB2)<-c("2,5%","97,5%")
rownames(KwantyleB2)<-colnames(B1WS)

for(i in 1:6){
  KwantyleB2[i,1]<-quantile(B2WS[,i],probs=.025)
  KwantyleB2[i,2]<-quantile(B2WS[,i],probs=.975)
}

B21<-ggplot(B2WS,aes(B2WS$`(Intercept)`))+
  geom_histogram(binwidth = 1,aes(fill=..count..))+
  geom_vline(data=B2WS,aes(xintercept = KwantyleB2[1,1]),color="forestgreen")+
  geom_vline(data=B2WS,aes(xintercept = KwantyleB2[1,2]),color="forestgreen")+
  theme_gdocs()+
  ylab("Liczność")+
  xlab("Wartości stałej")+
  ggtitle("Rozkład próby bootstrapowej dla stałej")+
  theme(plot.title=element_text(hjust=0.5))+
  scale_fill_gradient("Liczność",low="blue",high="red")

B22<-ggplot(B2WS,aes(B2WS$Moc))+
  geom_histogram(binwidth = 0.004,aes(fill=..count..))+
  geom_vline(data=B2WS,aes(xintercept = KwantyleB2[2,1]),color="forestgreen")+
  geom_vline(data=B2WS,aes(xintercept = KwantyleB2[2,2]),color="forestgreen")+
  theme_gdocs()+
  ylab("Liczność")+
  xlab("Wartości współczynnika")+
  ggtitle("Rozkład próby bootstrapowej\n dla zmiennej Moc")+
  theme(plot.title=element_text(hjust=0.5))+
  scale_fill_gradient("Liczność",low="blue",high="red")

B23<-ggplot(B2WS,aes(B2WS$WiekSamochodu))+
  geom_histogram(binwidth = 0.01,aes(fill=..count..))+
  geom_vline(data=B2WS,aes(xintercept = KwantyleB2[3,1]),color="forestgreen")+
  geom_vline(data=B2WS,aes(xintercept = KwantyleB2[3,2]),color="forestgreen")+
  theme_gdocs()+
  ylab("Liczność")+
  xlab("Wartości współczynnika")+
  ggtitle("Rozkład próby bootstrapowej\n dla zmiennej WiekSamochodu")+
  theme(plot.title=element_text(hjust=0.5))+
  scale_fill_gradient("Liczność",low="blue",high="red")

B24<-ggplot(B2WS,aes(B2WS$MiejsceProdukcji))+
  geom_histogram(binwidth = 0.05,aes(fill=..count..))+
  geom_vline(data=B2WS,aes(xintercept = KwantyleB2[4,1]),color="forestgreen")+
  geom_vline(data=B2WS,aes(xintercept = KwantyleB2[4,2]),color="forestgreen")+
  theme_gdocs()+
  ylab("Liczność")+
  xlab("Wartości współczynnika")+
  ggtitle("Rozkład próby bootstrapowej\n dla zmiennej MiejsceProdukcji")+
  theme(plot.title=element_text(hjust=0.5))+
  scale_fill_gradient("Liczność",low="blue",high="red")

B25<-ggplot(B2WS,aes(B2WS$ObjetoscSilnika))+
  geom_histogram(binwidth = 0.1,aes(fill=..count..))+
  geom_vline(data=B2WS,aes(xintercept = KwantyleB2[5,1]),color="forestgreen")+
  geom_vline(data=B2WS,aes(xintercept = KwantyleB2[5,2]),color="forestgreen")+
  theme_gdocs()+
  ylab("Liczność")+
  xlab("Wartości współczynnika")+
  ggtitle("Rozkład próby bootstrapowej\n dla zmiennej ObjetoscSilnika")+
  theme(plot.title=element_text(hjust=0.5))+
  scale_fill_gradient("Liczność",low="blue",high="red")

B26<-ggplot(B2WS,aes(B2WS$CzasPrzyspieszenia))+
  geom_histogram(binwidth = 0.02,aes(fill=..count..))+
  geom_vline(data=B1WS,aes(xintercept = KwantyleB2[6,1]),color="forestgreen")+
  geom_vline(data=B1WS,aes(xintercept = KwantyleB2[6,2]),color="forestgreen")+
  theme_gdocs()+
  ylab("Liczność")+
  xlab("Wartości współczynnika")+
  ggtitle("Rozkład próby bootstrapowej\n dla zmiennej CzasPrzyspieszenia")+
  theme(plot.title=element_text(hjust=0.5))+
  scale_fill_gradient("Liczność",low="blue",high="red")

grid.arrange(B21,B22,B23,B24,B25,B26, ncol = 2, nrow = 3)

# Przekształcenia regresji liniowej ---------------------------------------

#przekształcenie modelu do innej postaci
#tego bo jest lepiej dopasowany niż Hellwig

LogSpalanie<-log(CarsPL$Spalanie)
LogMoc<-log(CarsPL$Moc)
LogWiekSamochodu<-log(CarsPL$WiekSamochodu)
LogObjetoscSilnika<-log(CarsPL$ObjetoscSilnika)
LogCzasPrzyspieszenia<-log(CarsPL$CzasPrzyspieszenia)

LogCarsPL<-data.frame(LogSpalanie,LogMoc,LogWiekSamochodu,LogObjetoscSilnika,
                      LogCzasPrzyspieszenia)

RegresjaLLiniowa<-lm(LogSpalanie~LogMoc+LogWiekSamochodu+LogObjetoscSilnika+
                       LogCzasPrzyspieszenia,LogCarsPL)

#diagnostyka modelu

#przedziały ufności dla parametrów
RegresjaLLiniowaCI<-confint(RegresjaLLiniowa, level=0.95)

#błędy szacunku
sqrt(diag(vcov(RegresjaLLiniowa)))

#analiza reszt przekształconego modelu
ResztyRegresjiLLiniowej<-RegresjaLLiniowa$residuals

ggplot(RegresjaLLiniowa,aes(sample=ResztyRegresjiLLiniowej))+
  theme_gdocs()+
  stat_qq()+
  stat_qq_line()+
  ggtitle("Wykres Q-Q reszt modelu regresji po przekształceniach")+
  theme(plot.title=element_text(hjust=0.5))+
  xlab("Kwantyle teoretyczne")+
  ylab("Kwantyle z próby")

ggplot(data=RegresjaLLiniowa,aes(ResztyRegresjiLLiniowej))+
  geom_histogram(binwidth = 0.05, aes(y=..density.., fill=..count..))+
  theme_gdocs()+
  stat_function(fun=dnorm,color="forestgreen",size=1.25,
                args=list(mean=mean(ResztyRegresjiLLiniowej), 
                          sd=sd(ResztyRegresjiLLiniowej)))+
  ggtitle("Rozkład reszt modelu regresji po przekształceniach")+
  theme(plot.title=element_text(hjust=0.5))+
  xlab("Reszty modelu regresji")+
  ylab("Gęstość")+
  scale_fill_gradient("Liczność",low="blue",high="red")

jb.norm.test(ResztyRegresjiLLiniowej)
lillie.test(ResztyRegresjiLLiniowej)
shapiro.test(ResztyRegresjiLLiniowej)

#test Lillieforsa wykazał normalność reszt

#testowanie hetetoskedastyczności
bptest(LogSpalanie~LogMoc+LogWiekSamochodu+LogObjetoscSilnika+
         LogCzasPrzyspieszenia,data=LogCarsPL)

#test serii
runs.test(ResztyRegresjiLLiniowej)

#test stabilności Chowa
sctest(LogSpalanie~LogMoc+LogWiekSamochodu+LogObjetoscSilnika+
         LogCzasPrzyspieszenia,data=LogCarsPL, type="Chow", point=157)

#współliniowość
vif(RegresjaLLiniowa)

#koincydencja
MacierzKoincydencji<-as.data.frame(matrix(nrow=4,ncol=2,0))
colnames(MacierzKoincydencji)<-c("Współczynniki modelu", "Wartość współczynnika korelacji")
rownames(MacierzKoincydencji)<-c("log(Moc)","log(WiekSamochodu)",
                                 "log(ObjetoscSilnika)","log(CzasPrzyspieszenia)")

MacierzKoincydencji$`Współczynniki modelu`<-RegresjaLLiniowa$coefficients[2:5]

MacierzKoincydencji$`Wartość współczynnika korelacji`[1]<-cor(LogMoc,LogSpalanie)
MacierzKoincydencji$`Wartość współczynnika korelacji`[2]<-cor(LogWiekSamochodu,LogSpalanie)
MacierzKoincydencji$`Wartość współczynnika korelacji`[3]<-cor(LogObjetoscSilnika,LogSpalanie)
MacierzKoincydencji$`Wartość współczynnika korelacji`[4]<-cor(LogCzasPrzyspieszenia,LogSpalanie)
