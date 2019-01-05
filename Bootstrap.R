#Bootstrap project
#Created by: Patryk Zieliński, WZ AGH
#Kraków 2018

library(tidyr)
library(dplyr)
library(ggplot2)
library(psych)
library(ggcorrplot)
library(ggthemes)
library(leaps)
library(car)

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

#csv data
CarsCSV<-read.csv("https://raw.githubusercontent.com/getry28/RProjects/master/cars.csv",
                  header=T)

# Data import -------------------------------------------------------------


#data conversion
USGallon<- 3.785411784
Mile<-1.609344
Cal3<-0.02
Funt<-0.45359237
Kmh<-1.609344

CarsPL<-CarsCSV%>%
  mutate(mpg100PL = 100*USGallon/(Mile*mpg))%>%
  mutate(dispPL = Cal3*displacement)%>%
  mutate(weightPL = weight*Funt)%>%
  mutate(accPL = acceleration*100/(60*Kmh))%>%
  select(-c(mpg,displacement,weight,acceleration))

CarsPL$weightPL<-round(CarsPL$weightPL,digits = 0)
CarsPL$accPL<-round(CarsPL$accPL,digits=1)
CarsPL$mpg100PL<-round(CarsPL$mpg100PL,digits=1)

#descriptive statistic
CarsDS<-describe(CarsPL%>%
                   select(-name))%>%
  select(-c('vars','n','trimmed','mad','range','se'))%>%
  mutate(CoeffVar = sd/mean)%>%
  round(digits=3)

# Descriptive statistics creation -----------------------------------------

 
rownames(CarsDS)<-colnames(CarsPL %>%
                             select(-name))

#order df
col_order<-c("mpg100PL","name","cylinders","horsepower","year","origin",
             "dispPL","weightPL","accPL")

CarsPL<-CarsPL[,col_order]

#scatterplots
ggplot(CarsPL,aes(mpg100PL,dispPL,
                  color=cylinders, size=origin))+
  geom_jitter()+
  guides(
    color = guide_colourbar(order = 1),
    size = guide_legend(order = 2)
  )+
  ggtitle("Wykres rozrzutu objętości silnika względem spalania")+
  theme_gdocs()+
  theme(plot.title = element_text(hjust = 0.5))

# Scatterplots and correlation matrices -----------------------------------


ggplot(CarsPL,aes(mpg100PL,horsepower,
                  color=cylinders, size=origin))+
  geom_jitter()+
  guides(
    color = guide_colourbar(order = 1),
    size = guide_legend(order = 2)
  )+
  ggtitle("Wykres rozrzutu liczby koni mechanicznych względem spalania")+
  theme_gdocs()+
  theme(plot.title = element_text(hjust = 0.5))

ggplot(CarsPL,aes(mpg100PL,accPL,
                  color=cylinders, size=origin))+
  geom_jitter()+
  guides(
    color = guide_colourbar(order = 1),
    size = guide_legend(order = 2)
  )+
  ggtitle("Wykres rozrzutu przyspieszenia względem spalania")+
  theme_gdocs()+
  theme(plot.title = element_text(hjust = 0.5))

ggplot(CarsPL,aes(mpg100PL,weightPL,
                  color=cylinders, size=origin))+
  geom_jitter()+
  guides(
    color = guide_colourbar(order = 1),
    size = guide_legend(order = 2)
  )+
  ggtitle("Wykres rozrzutu wagi pojazdu względem spalania")+
  theme_gdocs()+
  theme(plot.title = element_text(hjust = 0.5))

#correlation matrix
CorrMatrix<-cor(CarsPL%>%
                  select(-name))

#correlation matrix plot
p.mat<-cor_pmat(CorrMatrix)
ggcorrplot(CorrMatrix,type="upper",outline.col="white",p.mat=p.mat)+
  ggtitle("Wyniki testu istotności współczynnika korelacji")+
  theme(plot.title=element_text(hjust=0.5))
ggcorrplot(CorrMatrix,type="upper",outline.col="white",lab=T)+
  ggtitle("Macierz korelacji zmiennych")+
  theme(plot.title=element_text(hjust=0.5))


#Hellwig method
CarsHellwig<-hellwig(CarsPL$mpg100PL,CarsPL %>%
          select(cylinders:accPL),method="pearson")%>%
  arrange(desc(h))

# Model with variables selected by Hellwig method -------------------------


variables<-CarsHellwig$k[1]%>%
  strsplit("-")%>%
  unlist()%>%
  as.numeric()

VarNames<-colnames(CarsPL[2+variables])

#Hellwig-based model
CarsHellwigLM<-lm(mpg100PL~cylinders+horsepower+
                    year+weightPL,CarsPL)

#model diagnostic
#CI for parameters
CarsHellwigLMCI<-confint(CarsHellwigLM, level=0.95)

#selecting best model from Hellwig method by R2 and RSQ
leaps<-regsubsets(mpg100PL~cylinders+horsepower+
                    year+weightPL,CarsPL,nbest=10)

# plot a table of models showing variables in each model.
# models are ordered by the selection statistic.
windows()
plot(leaps,scale="r2")
dev.off()
# plot statistic by subset size 
windows()
subsets(leaps, statistic="rsq")
dev.off()


