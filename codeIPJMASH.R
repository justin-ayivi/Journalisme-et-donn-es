rm(list=ls())
library(readxl)

setwd("D:/justro/DAUPHINE/Semestre 2/Journalisme et données/MASH - IPJ-20220209T083340Z-001/MASH - IPJ/")


commune_mont <- read_excel('revenu_patrimoine_900_v3.xlsx')



hist(commune_mont$patrimoine)

Jura <- commune_mont$patrimoine[commune_mont$Massif == "Jura"]
Alpes <- commune_mont$patrimoine[commune_mont$Massif == "Alpes"]
Pyrennees <- commune_mont$patrimoine[commune_mont$Massif == "PyrÃ©nÃ©es"]
Plaine <- commune_mont$patrimoine[commune_mont$Massif == "Plaine"]
Vosges <- commune_mont$patrimoine[commune_mont$Massif == "Vosges"]
MassCent <- commune_mont$patrimoine[commune_mont$Massif == "Massif Central"]
Martinique <- commune_mont$patrimoine[commune_mont$Massif == "Martinique"]
Reunion <- commune_mont$patrimoine[commune_mont$Massif == "RÃ©union"]
Corse <- commune_mont$patrimoine[commune_mont$Massif == "Corse"]


boxplot(commune_mont$patrimoine ~ commune_mont$Massif )


par(mfrow=c(2, 4))
hist(Jura)
hist(Alpes)
hist(Pyrennees)
hist(Vosges)
hist(MassCent)
hist(Martinique)
hist(Corse)
hist(Reunion)


######## graphes de statistiques descriptives

hist(commune_mont$patrimoine)
hist(commune_mont$`Part des impôts (%)`)

counts <-  table(commune_mont$Massif)

tt = length(commune_mont$Massif)


barplot(counts/tt)

library(ggplot2)


boxplot(commune_mont$patrimoine ~ commune_mont$Massif)

boxplot(commune_mont$patrimoine ~ commune_mont$`zone montagne`)

install.packages("tidyverse")

Massif <- commune_mont$patrimoine[commune_mont$`zone montagne` == "C"]
Plaine <- commune_mont$patrimoine[commune_mont$`zone montagne` == "NC"]

t.test(Massif, Plaine)
# différences significatives



# anova 

data_nc <-  commune_mont[commune_mont$"zone montagne" == "NC", ]
data_c <-  commune_mont[commune_mont$"zone montagne" == "C", ]
fit <- aov(commune_mont$patrimoine ~  commune_mont$Massif)
summary(fit)

fit2 <- aov(data_c$patrimoine ~  data_c$Massif)
summary(fit2)

TukeyHSD(fit2)
plot(TukeyHSD(fit2))



# il y'a des effets significatifs 

par(mfrow = c(1,2))

casse = seq(0,80, by = 2)

hist(data_c$patrimoine, breaks = casse, freq = F, ylim = range(0, 0.15))


hist(data_nc$patrimoine, breaks = casse, freq = F, ylim = range(0, 0.15))


library(fitdistrplus)

patrimoine_c <- data_c$patrimoine
fitc <- fitdist(patrimoine_c,"norm")
plot(fitc)
summary(fitc)

patrimoine_nc <- data_nc$patrimoine
fitnc <- fitdist(patrimoine_nc,"norm")
plot(fitnc)
summary(fitnc)



par(mfrow = c(1,2))
hist(data_c$patrimoine, breaks = casse, freq = F, ylim = range(0, 0.15))
curve(dnorm(x, fitc$estimate["mean"] , fitc$estimate["sd"] ), add=T, col=2)

hist(data_nc$patrimoine, breaks = casse, freq = F, ylim = range(0, 0.15))
curve(dnorm(x, fitnc$estimate["mean"], fitnc$estimate["sd"]), add = T, col = 3)


shapiro.test(data_c$patrimoine)

shapiro.test(data_nc$patrimoine)

# test de wilcoxon 

wilcox.test(data_c$patrimoine, data_nc$patrimoine)

wilcox.test(Massif, Plaine)

########################################


par(mfrow=c(2, 1))
Massif <- commune_mont$patrimoine[commune_mont$`zone montagne` == "C"]
Plaine <- commune_mont$patrimoine[commune_mont$`zone montagne` == "NC"]
hist(Plaine)
hist(Massif)

patrimoineC <- commune_mont$patrimoine[commune_mont$`zone montagne` == "C"]
patrimoineNC <- commune_mont$patrimoine[commune_mont$`zone montagne` == "NC"]

nbmenC <- commune_mont$`Nombre de ménages fiscaux`[commune_mont$`zone montagne` == "C"]
nbmenNC <- commune_mont$`Nombre de ménages fiscaux`[commune_mont$`zone montagne` == "NC"]


medianeC <- commune_mont$`Médiane du niveau de vie (???)`[commune_mont$`zone montagne` == "C"]
medianeNC <- commune_mont$`Médiane du niveau de vie (???)`[commune_mont$`zone montagne` == "NC"]


partimpotsC <- commune_mont$`Part des ménages fiscaux imposés (%)`[commune_mont$`zone montagne` == "C"]
partimpotsNC <- commune_mont$`Part des ménages fiscaux imposés (%)`[commune_mont$`zone montagne` == "NC"]


minimaC <- commune_mont$`dont part des minima sociaux (%)`[commune_mont$`zone montagne` == "C"]
minimaNC <- commune_mont$`dont part des minima sociaux (%)`[commune_mont$`zone montagne` == "NC"]


impotsC <- commune_mont$`Part des impôts (%)`[commune_mont$`zone montagne` == "C"]
impotsNC <- commune_mont$`Part des impôts (%)`[commune_mont$`zone montagne` == "NC"]



res1C <- cor.test(patrimoineC,nbmenC,method = "pearson")
res1NC <- cor.test(patrimoineNC,nbmenNC,method = "pearson")
res2C <- cor.test(patrimoineC,medianeC,method = "pearson")
res2NC <- cor.test(patrimoineNC,medianeNC,method = "pearson")




par(mfrow=c(1, 2))
plot(Massif,log(nbmenC),col = 'red',xlim = c(0,70),ylim = c(6,12))
abline(lm(Massif~I(log(nbmenC))))
plot(Plaine,log(nbmenNC),col = 'blue',xlim = c(0,70),ylim = c(6,12))
abline(lm(Plaine~I(log(nbmenNC))))


par(mfrow=c(1, 2))
plot(Massif,log(medianeC),col = 'red',xlim = c(0,20),ylim = c(9,11))
abline(lm(Massif~I(log(medianeC))))
plot(Plaine,log(medianeNC),col = 'blue',xlim = c(0,20),ylim = c(9,11))
abline(lm(Plaine~I(log(medianeNC))))



par(mfrow=c(1, 2))
plot(Massif,minimaC,col = 'red',xlim = c(0,20))
abline(lm(Massif~minimaC))
plot(Plaine,minimaNC,col = 'blue',xlim = c(0,20))
abline(lm(Plaine~log(minimaNC)))



par(mfrow=c(1, 2))
plot(Massif,impotsC,col = 'red',xlim = c(0,70),ylim = c(-35,0))
abline(lm(Massif~impotsC))
plot(Plaine,impotsNC,col = 'blue',xlim = c(0,70),ylim = c(-35,0))
abline(lm(Plaine~impotsNC))


par(mfrow=c(1, 2))
plot(Massif,partimpotsC,col = 'red')
abline(lm(Massif~partimpotsC))
plot(Plaine,partimpotsNC,col = 'blue')
abline(lm(Plaine~partimpotsNC))


par(mfrow=c(1, 2))
plot(Massif,log(partimpotsC),col = 'red',xlim = c(0,70),ylim = c(2.5,4.5))
abline(lm(Massif~I(log(partimpotsC))))
plot(Plaine,log(partimpotsNC),col = 'blue',xlim = c(0,70),ylim = c(2.5,4.5))
abline(lm(Plaine~I(log(partimpotsNC))))

