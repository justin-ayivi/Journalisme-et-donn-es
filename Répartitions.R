

rm(list=ls())
install.packages("readxl")
install.packages("fitdistrplus")
library("readxl")
library(fitdistrplus)
setwd("D:/justro/DAUPHINE/Semestre 2/Journalisme et données/MASH - IPJ-20220209T083340Z-001/MASH - IPJ/Base de données")

df = read_excel("revenu_patrimoine_900_v3.xlsx")
summary(df)

# relation base/hab, patrimoine
x = df$"Base TF/habitant"
y = df$"patrimoine"
reg = lm(y~x)
par(mfrow=c(1,1))
plot(x,y,pch=3, las=1)
abline(reg, lwd=2, col="blue")
par(mfrow=c(2,2))
plot(reg)
summary(reg)
confint(reg)
hist(x[x < 3000], 20)
hist(y[y < 30], 20)

# seulement montagne
x = df[df$"zone montagne" == "C","Base TF/habitant"]$'Base TF/habitant'
y = df[df$"zone montagne" == "C","patrimoine"]$'patrimoine'
reg = lm(y~x)
par(mfrow=c(1,1))
plot(x,y,pch=3, las=1)
abline(reg, lwd=2, col="blue")
par(mfrow=c(2,2))
plot(reg)

# seulement non montagne
x = df[df$"zone montagne" == "NC","Base TF/habitant"]$'Base TF/habitant'
y = df[df$"zone montagne" == "NC","patrimoine"]$'patrimoine'
reg = lm(y~x)
par(mfrow=c(1,1))
plot(x,y,pch=3, las=1)
abline(reg, lwd=2, col="blue")
par(mfrow=c(2,2))
plot(reg)


#répartition taux TF montagne / non montagne
TFC = df[df$"zone montagne" == "C","Taux TF"]$'Taux TF'
TFNC = df[df$"zone montagne" == "NC","Taux TF"]$'Taux TF'
par(mfrow=c(1,2))
casse = seq(0, 70, by = 5)
hist(TFC, breaks = casse, freq = F, ylim = range(0, 0.07))
hist(TFNC, breaks = casse, freq = F, ylim = range(0, 0.07))

descdist(TFC)
median(TFC)
median(TFNC)

fgC = fitdist(TFC, "gamma") 
plot(fgC)
fgNC = fitdist(TFNC, "gamma") 
plot(fgNC)


hist(TFC, breaks = casse, freq = F, ylim = range(0, 0.07))
curve(dgamma(x, fgC$estimate["shape"], fgC$estimate["rate"]), add=T, col=2)
curve(dgamma(x, fgNC$estimate["shape"], fgNC$estimate["rate"]), add=T, col=3)

fgC$estimate["shape"]







