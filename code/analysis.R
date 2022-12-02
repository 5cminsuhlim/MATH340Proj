## MAKE SURE TO SETWD TO INSIDE CODE FOLDER

## only over-the-board games
## mean_cp_less = beta_0 + beta_1 * age + beta_2 * time + beta_3 * elo + beta_4 * oppelo
## std_cp_less = beta_0 + beta_1 * age + beta_2 * time + beta_3 * elo + beta_4 * oppelo
## win vs. not win = beta_0 + beta_1 * blackelo + beta_2 * white_elo


## ref: https://stackoverflow.com/questions/5758084/loop-in-r-to-read-many-files
fi <- list.files('../data/',full.names=T)
dat <- lapply(fi,read.csv)

## ref: https://stackoverflow.com/questions/2851327/combine-a-list-of-data-frames-into-one-data-frame-by-row
library(dplyr)
players <- bind_rows(dat, .id = "column_label")


## individually
anand <- bind_rows(dat[2])
andreikin <- bind_rows(dat[3])
aronian <- bind_rows(dat[4])
bu <- bind_rows(dat[5])
carlsen <- bind_rows(dat[6])
caruana <- bind_rows(dat[7])
ding <- bind_rows(dat[8])
dominguezPerez <- bind_rows(dat[9])
duda <- bind_rows(dat[10])
eljanov <- bind_rows(dat[11])
erigaisi <- bind_rows(dat[12])
firouzja <- bind_rows(dat[13])
giri <- bind_rows(dat[14])
gukesh <- bind_rows(dat[15])
harikrishna <- bind_rows(dat[16])
karjakin <- bind_rows(dat[17])
le <- bind_rows(dat[18])
mamedyarov <- bind_rows(dat[19])
nakamura <- bind_rows(dat[20])
nepo <- bind_rows(dat[21])
niemann <- bind_rows(dat[22])
rapport <- bind_rows(dat[23])
so <- bind_rows(dat[24])
tomashevsky <- bind_rows(dat[25])
topalov <- bind_rows(dat[26])
vachierLagrave <- bind_rows(dat[27])
vallejoPons <- bind_rows(dat[28])
vitiugov <- bind_rows(dat[29])
wangH <- bind_rows(dat[30])
wei <- bind_rows(dat[31])
yu <- bind_rows(dat[32])


## EDA
## log (for right skewed)
## square/cube (for left skewed)
library(stringr)

names <- unique(players$Name)

# w/o normalization
par(mfrow=c(2,1))
for (name in names) {
        hist(players[players$Name == name,]$Mean_CP, xlab="Mean CP", main=paste("Mean CP Histogram for", str_to_title(name)))
        hist(players[players$Name == name,]$Std_CP, xlab="StD CP", main=paste("StD CP Histogram for", str_to_title(name)))
}


# w/ normalization
players$Mean_CP <- log(players$Mean_CP)
players$Std_CP <- log(players$Std_CP)

for (name in names) {
        hist(players[players$Name == name,]$Mean_CP, xlab="log(Mean CP)", main=paste("Log Mean CP Histogram for", str_to_title(name)))
        hist(players[players$Name == name,]$Std_CP, xlab="log(StD CP)", main=paste("Log StD CP Histogram for", str_to_title(name)))
}


## age
## lin reg (mean_cp)
par(mfrow=c(2,2))
for (name in names) {
        p.lm <- lm(Mean_CP ~ Age + Elo + OppElo, data=players[players$Name == name,])
        print(str_to_title(name))
        print(summary(p.lm))
        plot(p.lm)
}

## lin reg (std_cp)
for (name in names) {
        p.lm <- lm(Std_CP ~ Age + Elo + OppElo, data=players[players$Name == name,])
        print(str_to_title(name))
        print(summary(p.lm))
        plot(p.lm)
}

## time 
## lin reg (mean_cp)
for (name in names) {
        p.lm <- lm(Mean_CP ~ Time + Elo + OppElo, data=players[players$Name == name,])
        print(str_to_title(name))
        print(summary(p.lm))
        plot(p.lm)
}

## lin reg (std_cp)
for (name in names) {
        p.lm <- lm(Std_CP ~ Time + Elo + OppElo, data=players[players$Name == name,])
        print(str_to_title(name))
        print(summary(p.lm))
        plot(p.lm)
}


## all players

## EDA
par(mfrow=c(2,1))
hist(players$Mean_CP)
hist(players$Std_CP)

set.seed(123)
rand_samp_mean <- sample(players$Mean_CP, 5000)
shapiro.test(rand_samp_mean)
rand_samp_std <- sample(players$Std_CP, 5000)
shapiro.test(rand_samp_std)

qqnorm(players$Mean_CP)
qqline(players$Mean_CP, col = "red", lwd = 2)

qqnorm(players$Std_CP)
qqline(players$Std_CP, col = "red", lwd = 2)


<<<<<<< HEAD
## lin reg
players$WL <- factor(players$WL)
players$WhiteWL <- factor(players$WhiteWL)

## age
=======
nepo3.lm <- lm(nepo$Elo ~ nepo$Age + nepo$OppElo + nepo$Std_CP)
summary(nepo3.lm)
plot(nepo3.lm)

niemann3.lm <- lm(niemann$Elo ~ niemann$Age + niemann$OppElo + niemann$Std_CP)
summary(niemann3.lm)
plot(niemann3.lm)

## cumulative linear reg
players$WL <- factor(players$WL)
players$WhiteWL <- factor(players$WhiteWL)

>>>>>>> 97f8be49bc918da325bd0a42cd8e619a23eb0bea
players.lm <- lm(players$Mean_CP ~ players$Age + players$Elo + players$OppElo + players$WL)
summary(players.lm) ## intercept, elo, WL0, WL1

players2.lm <- lm(players$Std_CP ~ players$Age + players$Elo + players$OppElo + players$WL)
summary(players2.lm) ## intercept, elo, WL0, WL1

## time
players3.lm <- lm(players$Mean_CP ~ players$Time + players$Elo + players$OppElo + players$WL)
summary(players3.lm) ## intercept, time, elo, WL0, WL1

players4.lm <- lm(players$Std_CP ~ players$Time + players$Elo + players$OppElo + players$WL)
summary(players4.lm) ## intercept, time, WL0, WL1


## ordinal reg
library(MASS)
library(RStata)
library(modelsummary)
library(car)

## age
players.ord <- polr(WL ~ Age + Elo + OppElo + Mean_CP + Std_CP, data=players)
summary(players.ord)
vif(players.ord)
(ctable <- coef(summary(players.ord)))
p <- pnorm(abs(ctable[, "t value"]), lower.tail = FALSE) * 2
(ctable <- cbind(ctable, "p value" = p)) ## age, elo, oppelo, mean_cp, std_cp

players.ord2 <- polr(WhiteWL ~ Age + Elo + OppElo + Mean_CP + Std_CP, data=players)
summary(players.ord2)
(ctable <- coef(summary(players.ord2)))
p <- pnorm(abs(ctable[, "t value"]), lower.tail = FALSE) * 2
(ctable <- cbind(ctable, "p value" = p)) ## mean_cp, -1|0 intercept

## time
players.ord3 <- polr(WL ~ Age + Elo + OppElo + Mean_CP + Std_CP, data=players)
summary(players.ord3)
(ctable <- coef(summary(players.ord3)))
p <- pnorm(abs(ctable[, "t value"]), lower.tail = FALSE) * 2
(ctable <- cbind(ctable, "p value" = p)) ## age, elo, oppelo, mean_cp, std_cp

players.ord4 <- polr(WhiteWL ~ Age + Elo + OppElo + Mean_CP + Std_CP, data=players)
summary(players.ord4)
(ctable <- coef(summary(players.ord4)))
p <- pnorm(abs(ctable[, "t value"]), lower.tail = FALSE) * 2
(ctable <- cbind(ctable, "p value" = p)) ## mean_cp, -1|0 intercept

## model summary
mod = list("POLR_WL_age" = players.ord,
           "POLR_WhiteWL_age" = players.ord2,
           "POLR_WL_time" = players.ord3,
           "POLR_WhiteWL_time" = players.ord4)
modelsummary(mod, stars = TRUE)
