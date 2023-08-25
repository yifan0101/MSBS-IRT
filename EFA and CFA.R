# This study is a major project for the "Item Response Theory" course (self-funded) 
# Original statement: Code written by Meng Yifan and Cheng Yajing (Faculty of Psychology, Beijing Normal University)
# Some of the code is derived from the lecturer Tian Wei at the Collaborative Innovation Center.
# other questions please contact the author by email: 202011061075@mail.bnu.edu.cn
# Date Last Modified: Dec 7, 2022
library(psych)
setwd("C:\\Users\\Administrator\\Desktop\\项目反应理论\\结果报告练习数据")

item <- read.csv("item.csv",header = TRUE)
head(item)
describe(item)
summary(item)

itempoly <- polychoric(datanew)$rho
corPlot(itempoly,diag = F,zlim = c(.3,1),upper = FALSE,numbers = TRUE,cex.axis=.5)

cortest.bartlett(itempoly,n=500)
KMO(itempoly)

fa.parallel(itempoly, n.obs = 500, fa="pc", n.iter = 1000)
scree(itempoly,pc=T,factors = F, hline="-1", main="Scree Plot")

f3 <- fa(itempoly,nfactors=4, SMC=TRUE, fm="pa", residuals=TRUE, rotate="promax",n.obs=500)
print(f3,digits = 2,sort = TRUE)                            #载荷排序
resi_test <- residuals(f3,diag=FALSE)                       #相关矩阵残差
print(resi_test)
hist(resi_test,main = "",xlab = "Residuals")                #直方图

f2 <- fa(itempoly,nfactors=2, SMC=TRUE, fm="pa", residuals=TRUE, rotate="promax",n.obs=500)
print(f2,digits = 2,sort = TRUE)                            #载荷排序
resi_test <- residuals(f2,diag=FALSE)                       #相关矩阵残差
print(resi_test)
hist(resi_test,main = "",xlab = "Residuals")

###验证性因子分析
library(lavaan)
iq_mod <- '
inattention=~ M13 + B2
timeperception =~ A1 +  I9 +  V22
disengagement =~ S19 + G7 
higharouse=~  K11 + H8 +  E5 
'
iq_fit <- cfa(iq_mod, data = datanew)
inspect(iq_fit)
summary(iq_fit)
summary(iq_fit, standardized = TRUE, fit.measures = TRUE)
inspect(iq_fit, "rsquare")
