## MAKE SURE TO SETWD TO INSIDE CODE FOLDER

## only over-the-board games
## mean_cp_less = beta_0 + beta_1 * age + beta_2 * time + beta_3 * elo
## std_cp_less = beta_0 + beta_1 * age + beta_2 * time + beta_3 * elo

carlsen <- read.csv('../data/carlsen.csv')
erigaisi <- read.csv('../data/erigaisi.csv')
gukesh <- read.csv('../data/gukesh.csv')
nepo <- read.csv('../data/nepo.csv')
niemann <- read.csv('../data/niemann.csv')

par(mfrow=c(2,2))

hist(carlsen$Mean_CP)
hist(carlsen$Std_CP)
plot(carlsen$Mean_CP)
plot(carlsen$Std_CP)

hist(erigaisi$Mean_CP)
hist(erigaisi$Std_CP)
plot(erigaisi$Mean_CP)
plot(erigaisi$Std_CP)

hist(gukesh$Mean_CP)
hist(gukesh$Std_CP)
plot(gukesh$Mean_CP)
plot(gukesh$Std_CP)

hist(nepo$Mean_CP)
hist(nepo$Std_CP)
plot(nepo$Mean_CP)
plot(nepo$Std_CP)

hist(niemann$Mean_CP)
hist(niemann$Std_CP)
plot(niemann$Mean_CP)
plot(niemann$Std_CP)

dev.off()

