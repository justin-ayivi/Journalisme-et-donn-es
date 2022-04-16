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