library(mirt)
library(dplyr)
library(readxl)
setwd("C:\\Users\\Administrator\\Desktop\\项目反应理论\\项目反应理论大作业")
Data <- read_excel("Data.xls")
Data<-select(Data,-3,-6,-12,-14,-10,-15,-23)

freq <- apply(Data,2,table)
freq

spec<- 'inattention= M13,B2,Q17,R18,T20
timeperception =A1,I9,V22,D4
disengagement =S19,G7,P16,U21,X24
higharouse= K11,H8,E5
        COV=inattention*timeperception*disengagement*higharouse'

mpcm <- mirt(Data, model=spec, itemtype='gpcm', method = "MCEM", SE=F)

mgraded <- mirt(Data, model=spec, itemtype='graded',method = "MCEM", SE=F)
coefgraded<-coef(mgraded, simplify=T)
write.csv(coefgraded$items,file="GRM.csv")
anova(mpcm, mgraded)
coefgraded$'items'

dm <- coefgraded$'items'[,5:10] 
b1 <- -(dm[,1]) 
b2 <- -(dm[,2]-(dm[,1]))
b3 <- -(dm[,3]-(dm[,2]))
b4 <- -(dm[,4]-(dm[,3]))
b5 <- -(dm[,5]-(dm[,4]))
b6 <- -(dm[,6]-(dm[,5]))
b  <- cbind(b1,b2,b3,b4,b5,b6)
round(b,2)
write.csv(b,file="GRMb.csv")

itemplot(mgraded, drop.zeros = TRUE,xlim=c(-3,3)) #drop the zero slopes to allow plotting
itemplot(mgraded,14, drop.zeros = TRUE, type = 'info',xlim=c(-3,3),ylim=c(0,3)) #drop the zero slopes to allow plotting

itemfit(mgraded,na.rm = TRUE,QMC=TRUE)

M2(obj = mgraded, QMC=TRUE)
M2(obj = mpcm, QMC=TRUE)

Q3<- residuals(object = mgraded,"Q3",QMC=TRUE)
LD<- residuals(object = mgraded,"LD",QMC=TRUE)
write.csv(Q3,file="Q3(4).csv")
datanew<-select(Data,-3,-10,-15,-17,-11,-12,-14)
