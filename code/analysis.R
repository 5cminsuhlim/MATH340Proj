## MAKE SURE TO SETWD TO INSIDE CODE FOLDER

## only over-the-board games
## mean_cp_less = beta_0 + beta_1 * age + beta_2 * time + beta_3 * elo + beta_4 * oppelo
## std_cp_less = beta_0 + beta_1 * age + beta_2 * time + beta_3 * elo + beta_4 * oppelo
## win vs. not win = beta_0 + beta_1 * blackelo + beta_2 * white_elo
## 

carlsen <- read.csv('../data/carlsen.csv')
erigaisi <- read.csv('../data/erigaisi.csv')
gukesh <- read.csv('../data/gukesh.csv')
nepo <- read.csv('../data/nepo.csv')
niemann <- read.csv('../data/niemann.csv')


## EDA
par(mfrow=c(2,3))
plot(carlsen$Mean_CP, carlsen$Std_CP)
plot(nepo$Mean_CP, nepo$Std_CP)
plot(gukesh$Mean_CP, gukesh$Std_CP)
plot(erigaisi$Mean_CP, erigaisi$Std_CP)
plot(niemann$Mean_CP, niemann$Std_CP)
dev.off()

par(mfrow=c(2,3))
plot(carlsen$Elo, carlsen$Mean_CP)
plot(nepo$Elo, nepo$Mean_CP)
plot(gukesh$Elo, gukesh$Mean_CP)
plot(erigaisi$Elo, erigaisi$Mean_CP)
plot(niemann$Elo, niemann$Mean_CP)
dev.off()

par(mfrow=c(2,3))
boxplot(carlsen$Mean_CP)
boxplot(nepo$Mean_CP)
boxplot(gukesh$Mean_CP)
boxplot(erigaisi$Mean_CP)
boxplot(niemann$Mean_CP)
dev.off()

par(mfrow=c(2,3))
boxplot(carlsen$Std_CP)
boxplot(nepo$Std_CP)
boxplot(gukesh$Std_CP)
boxplot(erigaisi$Std_CP)
boxplot(niemann$Std_CP)
dev.off()



## lin reg 1
carlsen.lm <- lm(carlsen$Std_CP ~ carlsen$Age + carlsen$Elo + carlsen$OppElo + carlsen$WL)
summary(carlsen.lm)
plot(carlsen.lm)

erigaisi.lm <- lm(erigaisi$Std_CP ~ erigaisi$Age + erigaisi$Elo + erigaisi$OppElo + erigaisi$WL)
summary(erigaisi.lm)
plot(erigaisi.lm)

gukesh.lm <- lm(gukesh$Std_CP ~ gukesh$Age + gukesh$Elo + gukesh$OppElo + gukesh$WL)
summary(gukesh.lm)
plot(gukesh.lm)

nepo.lm <- lm(nepo$Std_CP ~ nepo$Age + nepo$Elo + nepo$OppElo + nepo$WL)
summary(nepo.lm)
plot(nepo.lm)

niemann.lm <- lm(niemann$Std_CP ~ niemann$Age + niemann$Elo + niemann$OppElo + niemann$WL)
summary(niemann.lm)
plot(niemann.lm)

## lin reg 2
carlsen2.lm <- lm(carlsen$Elo ~ carlsen$Age + carlsen$OppElo + carlsen$Mean_CP)
summary(carlsen2.lm)
plot(carlsen2.lm)

erigaisi2.lm <- lm(erigaisi$Elo ~ erigaisi$Age + erigaisi$OppElo + erigaisi$Mean_CP)
summary(erigaisi2.lm)
plot(erigaisi2.lm)

gukesh2.lm <- lm(gukesh$Elo ~ gukesh$Age + gukesh$OppElo + gukesh$Mean_CP)
summary(gukesh2.lm)
plot(gukesh2.lm)

nepo2.lm <- lm(nepo$Elo ~ nepo$Age + nepo$OppElo + nepo$Mean_CP)
summary(nepo2.lm)
plot(nepo2.lm)

niemann2.lm <- lm(niemann$Elo ~ niemann$Age + niemann$OppElo + niemann$Mean_CP)
summary(niemann2.lm)
plot(niemann2.lm)

## lin reg 3
carlsen3.lm <- lm(carlsen$Elo ~ carlsen$Age + carlsen$OppElo + carlsen$Std_CP)
summary(carlsen3.lm)
plot(carlsen3.lm)

erigaisi3.lm <- lm(erigaisi$Elo ~ erigaisi$Age + erigaisi$OppElo + erigaisi$Std_CP)
summary(erigaisi3.lm)
plot(erigaisi3.lm)

gukesh3.lm <- lm(gukesh$Elo ~ gukesh$Age + gukesh$OppElo + gukesh$Std_CP)
summary(gukesh3.lm)
plot(gukesh3.lm)

nepo3.lm <- lm(nepo$Elo ~ nepo$Age + nepo$OppElo + nepo$Std_CP)
summary(nepo3.lm)
plot(nepo3.lm)

niemann3.lm <- lm(niemann$Elo ~ niemann$Age + niemann$OppElo + niemann$Std_CP)
summary(niemann3.lm)
plot(niemann3.lm)

## cumulative linear reg
players <- rbind(carlsen, erigaisi, gukesh, nepo, niemann)
players$WL <- factor(players$WL)

players.lm <- lm(players$Mean_CP ~ players$Age + players$Elo + players$OppElo + players$WL)
summary(players.lm)

players2.lm <- lm(players$Std_CP ~ players$Age + players$Elo + players$OppElo + players$WL)
summary(players2.lm)

library(MASS)
players.ord <- polr(WL ~ Elo + OppElo, data=players)
summary(players.ord)

## ordinal reg, go through every player, get white black elo, win loss for white ## ERIC


## deeptha + alex
## ridge reg w/ cp_loss

## optimal model
library(leaps)
p.leapsadj <- leaps(y=players$Std_CP, x=players[,c(1,2,3,5,6,7)], nbest=2, method='adjr2')
p.leapsfulladj <- cbind(p.leapsadj$which, p.leapsadj$adjr2)
p.leapsfulladj
