#Panel Data Analysis
#BBVA Liga - 2017/18
#Created by: Patryk Zielinski, WZ AGH
library(xlsx)
library(httr)
library(psych)
library(dplyr)
library(ggcorrplot)
library(ggplot2)
library(scales)
library(plm)
library(lmtest)
library(nortest)
library(normtest)

#data import
url <- "https://github.com/getry28/RProjects/blob/master/EW_BBVA.xlsx?raw=true"
GET(url, write_disk("EW_BBVA.xlsx", overwrite=TRUE))
BBVAData <- read.xlsx("EW_BBVA.xlsx", sheetIndex=1, header=TRUE)

# Descriptive Statistics and Correlation ----------------------------------

#descriptive statistics
DStatistics<-describe(BBVAData %>%
    select(GoalsScored:PassesAccuracy))%>%
    select(-c('n','vars','trimmed','mad','range','se'))%>%
    mutate(CoeffVar = sd/mean)%>%
    round(digits=2)

rownames(DStatistics)<-colnames(BBVAData %>%
           select(GoalsScored:PassesAccuracy))

#correlation matrix
CorrMatrix<-cor(BBVAData %>%
                   select(GoalsScored:PassesAccuracy))%>%
  round(digits=2)

#correlation matrix plot
p.mat<-cor_pmat(CorrMatrix)
ggcorrplot(CorrMatrix,type="upper",outline.col="white",p.mat=p.mat)+
  ggtitle("Wyniki testu istotności współczynnika korelacji")+
  theme(plot.title=element_text(hjust=0.5))
ggcorrplot(CorrMatrix,type="upper",outline.col="white",lab=T)+
  ggtitle("Macierz korelacji zmiennych")+
  theme(plot.title=element_text(hjust=0.5))


# Varables plots ----------------------------------------------------------

#variables plots by group
#GoalsScored
ggplot(BBVAData,aes(x=GameNo,y=GoalsScored,color=factor(GameID)))+
  geom_line()+
  geom_point()+
  ggtitle("Wykres szeregu zmiennej GoalsScored \nw ciągu całego sezonu")+
  theme(plot.title=element_text(hjust=0.5))+
  labs(color="GameID")+
  scale_y_continuous(breaks=seq(1,10,by=2))+
  scale_x_continuous(breaks=seq(0,38,by=2))

#Shots
ggplot(BBVAData,aes(x=GameNo,y=Shots,color=factor(GameID)))+
  geom_line()+
  geom_point()+
  ggtitle("Wykres szeregu zmiennej Shots \nw ciągu całego sezonu")+
  theme(plot.title=element_text(hjust=0.5))+
  labs(color="GameID")+
  scale_y_continuous(breaks=seq(10,50,by=5))+
  scale_x_continuous(breaks=seq(0,38,by=2))

#ShotsOnTarget
ggplot(BBVAData,aes(x=GameNo,y=ShotsOnTarget,color=factor(GameID)))+
  geom_line()+
  geom_point()+
  ggtitle("Wykres szeregu zmiennej ShotsOnTarget \nw ciągu całego sezonu")+
  theme(plot.title=element_text(hjust=0.5))+
  labs(color="GameID")+
  scale_y_continuous(breaks=seq(0,20,by=4))+
  scale_x_continuous(breaks=seq(0,38,by=2))

#FoulsCommitted
ggplot(BBVAData,aes(x=GameNo,y=FoulsCommitted,color=factor(GameID)))+
  geom_line()+
  geom_point()+
  ggtitle("Wykres szeregu zmiennej FoulsCommitted \nw ciągu całego sezonu")+
  theme(plot.title=element_text(hjust=0.5))+
  labs(color="GameID")+
  scale_y_continuous(breaks=seq(10,50,by=5))+
  scale_x_continuous(breaks=seq(0,38,by=2))

#Corners
ggplot(BBVAData,aes(x=GameNo,y=Corners,color=factor(GameID)))+
  geom_line()+
  geom_point()+
  ggtitle("Wykres szeregu zmiennej Corners \nw ciągu całego sezonu")+
  theme(plot.title=element_text(hjust=0.5))+
  labs(color="GameID")+
  scale_y_continuous(breaks=seq(0,20,by=4))+
  scale_x_continuous(breaks=seq(0,38,by=2))

#YellowCards
ggplot(BBVAData,aes(x=GameNo,y=YellowCards,color=factor(GameID)))+
  geom_line()+
  geom_point()+
  ggtitle("Wykres szeregu zmiennej YellowCards \nw ciągu całego sezonu")+
  theme(plot.title=element_text(hjust=0.5))+
  labs(color="GameID")+
  scale_y_continuous(breaks=seq(0,15,by=3))+
  scale_x_continuous(breaks=seq(0,38,by=2))

#RedCards
ggplot(BBVAData,aes(x=GameNo,y=RedCards,color=factor(GameID)))+
  geom_line()+
  geom_point()+
  ggtitle("Wykres szeregu zmiennej RedCards \nw ciągu całego sezonu")+
  theme(plot.title=element_text(hjust=0.5))+
  labs(color="GameID")+
  scale_y_continuous(breaks=seq(0,3,by=1))+
  scale_x_continuous(breaks=seq(0,38,by=2))

#Offsides
ggplot(BBVAData,aes(x=GameNo,y=Offsides,color=factor(GameID)))+
  geom_line()+
  geom_point()+
  ggtitle("Wykres szeregu zmiennej Offsides \nw ciągu całego sezonu")+
  theme(plot.title=element_text(hjust=0.5))+
  labs(color="GameID")+
  scale_y_continuous(breaks=seq(0,20,by=4))+
  scale_x_continuous(breaks=seq(0,38,by=2))

#HomeBallPosession
ggplot(BBVAData,aes(x=GameNo,y=HomeBallPosession,color=factor(GameID)))+
  geom_line()+
  geom_point()+
  ggtitle("Wykres szeregu zmiennej HomeBallPosession \nw ciągu całego sezonu")+
  theme(plot.title=element_text(hjust=0.5))+
  labs(color="GameID")+
  scale_y_continuous(labels=percent)+
  scale_x_continuous(breaks=seq(0,38,by=2))

#Passes
ggplot(BBVAData,aes(x=GameNo,y=Passes,color=factor(GameID)))+
  geom_line()+
  geom_point()+
  ggtitle("Wykres szeregu zmiennej Passes \nw ciągu całego sezonu")+
  theme(plot.title=element_text(hjust=0.5))+
  labs(color="GameID")+
  scale_y_continuous(breaks=seq(570,1170,by=100))+
  scale_x_continuous(breaks=seq(0,38,by=2))

#PassesAccuracy
ggplot(BBVAData,aes(x=GameNo,y=PassesAccuracy,color=factor(GameID)))+
  geom_line()+
  geom_point()+
  ggtitle("Wykres szeregu zmiennej PassesAccuracy \nw ciągu całego sezonu")+
  theme(plot.title=element_text(hjust=0.5))+
  labs(color="GameID")+
  scale_y_continuous(labels=percent)+
  scale_x_continuous(breaks=seq(0,38,by=2))


# Scatterplots ------------------------------------------------------------

#scatterplots
#Shots
ggplot(BBVAData,aes(GoalsScored,Shots))+
  geom_jitter(aes(color=factor(GameID)))+
  geom_smooth(method="lm",se=F)+
  labs(color="GameID")+
  ggtitle("Wykres rozrzutu liczby strzałów względem liczby strzelonych goli")+
  theme(plot.title=element_text(hjust=0.5))+
  scale_x_continuous(breaks=seq(1,10,by=2))

#ShotsOnTaget
ggplot(BBVAData,aes(GoalsScored,ShotsOnTarget))+
  geom_jitter(aes(color=factor(GameID)))+
  geom_smooth(method="lm",se=F)+
  labs(color="GameID")+
  ggtitle("Wykres rozrzutu liczby strzałów celnych względem liczby strzelonych goli")+
  theme(plot.title=element_text(hjust=0.5))+
  scale_x_continuous(breaks=seq(1,10,by=2))

#FoulsCommitted
ggplot(BBVAData,aes(GoalsScored,FoulsCommitted))+
  geom_jitter(aes(color=factor(GameID)))+
  geom_smooth(method="lm",se=F)+
  labs(color="GameID")+
  ggtitle("Wykres rozrzutu liczby fauli względem liczby strzelonych goli")+
  theme(plot.title=element_text(hjust=0.5))+
  scale_x_continuous(breaks=seq(1,10,by=2))

#Corners
ggplot(BBVAData,aes(GoalsScored,Corners))+
  geom_jitter(aes(color=factor(GameID)))+
  geom_smooth(method="lm",se=F)+
  labs(color="GameID")+
  ggtitle("Wykres rozrzutu liczby rzutów rożnych względem liczby strzelonych goli")+
  theme(plot.title=element_text(hjust=0.5))+
  scale_x_continuous(breaks=seq(1,10,by=2))

#YellowCards
ggplot(BBVAData,aes(GoalsScored,YellowCards))+
  geom_jitter(aes(color=factor(GameID)))+
  geom_smooth(method="lm",se=F)+
  labs(color="GameID")+
  ggtitle("Wykres rozrzutu liczby żółtych kartek względem liczby strzelonych goli")+
  theme(plot.title=element_text(hjust=0.5))+
  scale_x_continuous(breaks=seq(1,10,by=2))+
  scale_y_continuous(breaks=seq(0,15,by=3))

#RedCards
ggplot(BBVAData,aes(GoalsScored,RedCards))+
  geom_jitter(aes(color=factor(GameID)))+
  geom_smooth(method="lm",se=F)+
  labs(color="GameID")+
  ggtitle("Wykres rozrzutu liczby czerwonych kartek względem liczby strzelonych goli")+
  theme(plot.title=element_text(hjust=0.5))+
  scale_x_continuous(breaks=seq(1,10,by=2))+
  scale_y_continuous(breaks=seq(0,2,by=1))

#Offsides
ggplot(BBVAData,aes(GoalsScored,Offsides))+
  geom_jitter(aes(color=factor(GameID)))+
  geom_smooth(method="lm",se=F)+
  labs(color="GameID")+
  ggtitle("Wykres rozrzutu liczby spalonych względem liczby strzelonych goli")+
  theme(plot.title=element_text(hjust=0.5))+
  scale_x_continuous(breaks=seq(1,10,by=2))

#HomeBallPosession
ggplot(BBVAData,aes(GoalsScored,HomeBallPosession))+
  geom_jitter(aes(color=factor(GameID)))+
  geom_smooth(method="lm",se=F)+
  labs(color="GameID")+
  ggtitle("Wykres rozrzutu stosunku posiadania piłki przez drużynę \ngospodarzy względem liczby strzelonych goli")+
  theme(plot.title=element_text(hjust=0.5))+
  scale_x_continuous(breaks=seq(1,10,by=2))+
  scale_y_continuous(labels=percent)

#Passes
ggplot(BBVAData,aes(GoalsScored,Passes))+
  geom_jitter(aes(color=factor(GameID)))+
  geom_smooth(method="lm",se=F)+
  labs(color="GameID")+
  ggtitle("Wykres rozrzutu liczby podań względem liczby strzelonych goli")+
  theme(plot.title=element_text(hjust=0.5))+
  scale_x_continuous(breaks=seq(1,10,by=2))

#PassesAccuracy
ggplot(BBVAData,aes(GoalsScored,PassesAccuracy))+
  geom_jitter(aes(color=factor(GameID)))+
  geom_smooth(method="lm",se=F)+
  labs(color="GameID")+
  ggtitle("Wykres rozrzutu stosunku celnych podań względem liczby strzelonych goli")+
  theme(plot.title=element_text(hjust=0.5))+
  scale_x_continuous(breaks=seq(1,10,by=2))+
  scale_y_continuous(labels=percent)

# Panel models ------------------------------------------------------------

#create panel data frame
BBVADataP <- pdata.frame(BBVAData,index=c("GameID","GameNo"),drop.index = T)

#pooling model
BBVAPooling<-plm(GoalsScored~Shots+ShotsOnTarget+FoulsCommitted+
                   Corners+YellowCards+RedCards+Offsides+
                   HomeBallPosession+Passes,
                 data=BBVADataP,model="pooling")
summary(BBVAPooling)

#first lags for variables
Corners1<-lag(BBVADataP$Corners,1)
YellowCards1<-lag(BBVADataP$YellowCards,1)
RedCards1<-lag(BBVADataP$RedCards,1)
Offsides1<-lag(BBVADataP$Offsides,1)
HomeBallPosession1<-lag(BBVADataP$HomeBallPosession,1)
Passes1<-lag(BBVADataP$Passes,1)

BBVAPooling1<-plm(GoalsScored~Shots+ShotsOnTarget+FoulsCommitted+
                    Corners1+YellowCards1+RedCards1+Offsides1+
                    HomeBallPosession1+Passes1
                  ,data=BBVADataP,model="pooling")
summary(BBVAPooling1)

#FE model
BBVAFE<-plm(GoalsScored~Shots+ShotsOnTarget+FoulsCommitted+
              Corners+YellowCards+RedCards+Offsides+
              HomeBallPosession+Passes,
            data=BBVADataP,model="within")
summary(BBVAFE)

#RE model
BBVARE<-plm(GoalsScored~Shots+ShotsOnTarget+FoulsCommitted+
              Corners+YellowCards+RedCards+Offsides+
              HomeBallPosession+Passes,
            data=BBVAData,model="random")
#RE IS IMPOSSIBLE TO CREATE
#niemożliwe oszacowanie średnich grupowych regresji
#macierz niemożliwa do odwrócenia

#diagnostyka

#wyodrębnienie efektów indywidualnych - wg przekroju
summary(fixef(BBVAFE,type="dmean"))

#wyodrębnienie efektów okresowych
twoway<-plm(GoalsScored~Shots+ShotsOnTarget+FoulsCommitted+
              Corners+YellowCards+RedCards+Offsides+
              HomeBallPosession+Passes,
            data=BBVADataP,model="within",effect = "time")
summary(fixef(twoway,effect="time"))


#H0: brak wpływu czasu
plmtest(BBVAFE, c("time"), type=("bp"))

plmtest(BBVAFE,"individual",type="bp")

GameNo6<-BBVAData %>%
  select(GameID:Passes)%>%
  filter(GameNo==6)

GameNo21<-BBVAData %>%
  select(GameID:Passes)%>%
  filter(GameNo==21)

mean(GameNo6$GoalsScored)
mean(GameNo21$GoalsScored)

ggplot(GameNo6,aes(GameID,GoalsScored))+
  geom_line()+
  ggtitle("Wykres liczby zdobytych goli w meczach 6. kolejki BBVA Liga")+
  theme(plot.title=element_text(hjust=0.5))+
  scale_x_continuous(breaks=seq(1,10,by=1))

ggplot(GameNo21,aes(GameID,GoalsScored))+
  geom_line()+
  ggtitle("Wykres liczby zdobytych goli w meczach 21. kolejki BBVA Liga")+
  theme(plot.title=element_text(hjust=0.5))+
  scale_x_continuous(breaks=seq(1,10,by=1))

#H0: lepszy jest OLS ze względu na brak zróżnicowania wyrazu wolnego w grupach
pFtest(BBVAFE,BBVAPooling)

#w moim przypadku lepszy jest OLS

#dobór zmiennych do modelu
BBVAPooling10<-plm(GoalsScored~Shots+ShotsOnTarget+FoulsCommitted,
                 data=BBVADataP,model="pooling")
summary(BBVAPooling10)

#cross-sectional dependence - group heteroskedascity tests
pcdtest(BBVAPooling10,test="lm")
pcdtest(BBVAPooling10,test="cd")

#serial correlation testing - korelacja międzygrupowa
pbgtest(BBVAPooling10)

#normalność reszt
shapiro.test(BBVAPooling10$residuals)
jb.norm.test(BBVAPooling10$residuals)

#autokorelacja
BBVAResiduals<-BBVAPooling10$residuals
Residuals1<-lag(BBVAResiduals,1)

AutoCorrM<-lm(BBVAResiduals~Residuals1)
summary(AutoCorrM)

BBVAResiduals2<-BBVAResiduals^2
WhiteModel<-lm(BBVAResiduals2~GoalsScored+(GoalsScored*GoalsScored),data=BBVAData)
summary(WhiteModel)


# Other functions ---------------------------------------------------------

multiplot <- function(..., plotlist=NULL, file, cols=1, layout=NULL) {
  library(grid)
  
  # Make a list from the ... arguments and plotlist
  plots <- c(list(...), plotlist)
  
  numPlots = length(plots)
  
  # If layout is NULL, then use 'cols' to determine layout
  if (is.null(layout)) {
    # Make the panel
    # ncol: Number of columns of plots
    # nrow: Number of rows needed, calculated from # of cols
    layout <- matrix(seq(1, cols * ceiling(numPlots/cols)),
                     ncol = cols, nrow = ceiling(numPlots/cols))
  }
  
  if (numPlots==1) {
    print(plots[[1]])
    
  } else {
    # Set up the page
    grid.newpage()
    pushViewport(viewport(layout = grid.layout(nrow(layout), ncol(layout))))
    
    # Make each plot, in the correct location
    for (i in 1:numPlots) {
      # Get the i,j matrix positions of the regions that contain this subplot
      matchidx <- as.data.frame(which(layout == i, arr.ind = TRUE))
      
      print(plots[[i]], vp = viewport(layout.pos.row = matchidx$row,
                                      layout.pos.col = matchidx$col))
    }
  }
}