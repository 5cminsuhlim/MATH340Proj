library(ggplot2)
library(QuantPsyc)

#ANOVA & MANOVA

##Data Processing

#players.2 <- filter(players, players$Elo != 0) #filtering out players with 0 elo
players.2 <- players[,c(4:9, 11)]

players.niemann <- players.2
players.niemann$Name <- ifelse(players.niemann$Name=="niemann", "niemann","other") #dataset to compare niemann vs others

##Checking Assumptions

library(car)
library(mvnormtest)

leveneTest(Mean_CP ~ Name, data = players.2)
leveneTest(Std_CP ~ Name, data = players.2)

players.sample <- players.2[sample(nrow(players.2), 4999), c(1:5)]
mshapiro.test(t(players.sample))

players.2$mahal <- mahalanobis(players.2[,-7], colMeans(players.2[,-7]), cov(players.2[,-7])) #checking for multivariate outliers
players.2$mahal_p <- pchisq(players.2$mahal, df=5, lower.tail=FALSE) #a lot of outliers, so assumptions technically not met? 

##MANVOA with ACPL and SDCPL for all players

players.manova <- manova(as.matrix(players.2[,2:3]) ~ players.2$Name)
summary(players.manova)
summary(players.manova, test = "Wilks")
summary(players.manova, test = "Hotelling-Lawley")
summary(players.manova, test = "Roy")

##MANVOA with ACPL and SDCPL for Niemann vs the rest

players.niemann.manova <- manova(as.matrix(players.niemann[,2:3]) ~ players.niemann$Name)
summary(players.niemann.manova)
summary(players.niemann.manova, test = "Wilks")
summary(players.niemann.manova, test = "Hotelling-Lawley")
summary(players.niemann.manova, test = "Roy")

##MANVOA with ACPL and SDCPL with multiple IVs

players.niemann.manova.twoway <- manova(as.matrix(players.niemann[,2:3]) ~ players.niemann$Name * as.factor(players.niemann$Time))
summary(players.niemann.manova.twoway)

###setting up elo as a categorical variable

players.niemann$elo.cat <- ifelse(players.niemann$Elo < 2700, "low",
                                  ifelse(players.niemann$Elo >= 2700 & players.niemann$Elo < 2775, "med",
                                        ifelse(players.niemann$Elo >= 2775, "high", "NA")))


players.niemann$elo.cat <- factor(players.niemann$elo.cat, levels = c("low","med","high"))



###MANOVA with ACPL and SDCPL with multiple IVs, including categorical elo

players.niemann.manova.twoway.elo <- manova(as.matrix(players.niemann[,2:3]) ~ players.niemann$Name * as.factor(players.niemann$Elo))
summary(players.niemann.manova.twoway.elo) # testing for interaction

players.niemann.manova.twoway.elo2 <- manova(as.matrix(players.niemann[,2:3]) ~ players.niemann$Name + as.factor(players.niemann$Elo))
summary(players.niemann.manova.twoway.elo2) 

summary.aov(players.niemann.manova.twoway.elo)

###Some accompanying visualization

ggplot() + geom_point(data = players.niemann[players.niemann$Name=="other",], aes(x = Mean_CP, y = Std_CP), color = "gray", alpha = 0.2) +
  geom_point(data = players.niemann[players.niemann$Name=="niemann",], aes(x = Mean_CP, y = Std_CP, color = elo.cat)) + 
  #scale_color_gradient(low = "white", high = "darkred") +
  theme_minimal()

ggplot() + geom_boxplot(data = players.niemann, aes(x = Name, y = log(Mean_CP), fill = elo.cat)) + theme_minimal()

#########################

#Influence Measures

players.lm <- lm(log(Mean_CP) ~ Elo + Time + WL + Std_CP, data = players.2)
summary(players.lm)

influence.players.lm <- influence.measures(players.lm)
plot(players.lm, which = 4)

#########################

#creating a dataset that merges games by player-year

playeryear <- summarize(group_by(players, Name, Time),
                        mean_acpl = mean(Mean_CP),
                        mean_sdcpl = mean(Std_CP),
                        win_ratio = mean(WL),
                        Elo = mean(Elo),
                        OppElo = mean(OppElo))

player.elo <- summarize(group_by(players, Name, elo.cat),
                        mean_acpl = mean(Mean_CP),
                        mean_sdcpl = mean(Std_CP),
                        win_ratio = mean(WL))

#####################################

ggplot(data = playeryear[playeryear$Elo != 0,]) + geom_point(aes(x = Elo, y = mean_sdcpl, color = mean_acpl)) +
  theme_minimal() + scale_color_gradient(low = "lightblue", high = "darkblue")

#player-year regression, mean avg centipawn loss ~ elo 

acpl.lm <- lm(mean_acpl ~ Elo + OppElo + win_ratio + Time, data = playeryear)
summary(acpl.lm)

ols_step_all_possible(acpl.lm) #getting optimal model
ols_step_best_subset(acpl.lm) 

#WIS?
#plot(playeryear[playeryear$Elo != 0,]$Elo, abs(acpl.lm$resid))

#looking for influential points with influence measures: 4, 10, 11, 12, 30, 31, 42, 43, 44, 64 (niemann), 65 (niemann), 97
influence.measures(acpl.lm) 

#looking for influential points with residual plot
playeryear$fitted <- acpl.lm$fitted.values
playeryear$resid <- acpl.lm$residuals

#looking for outlying hat values
playeryear$hats <- hatvalues(acpl.lm)
hatthreshold <- 3*2/nrow(playeryear)

#plotting influential points
ggplot() + geom_point(data = playeryear, aes(x = fitted, y = resid), color = "gray", alpha = 0.5) +   
  geom_text(data = playeryear, aes(x = fitted, y = resid+0.2, label = ifelse(abs(playeryear$resid) >= 10, paste(playeryear$Name, playeryear$Time, sep = " "), "")), size = 3, color = "blue") + 
  geom_text(data = playeryear, aes(x = fitted, y = resid-0.2, label = ifelse(abs(playeryear$hats) >= hatthreshold, paste(playeryear$Name, playeryear$Time, sep = " "), "")), size = 3, color = "darkgreen") + 
  theme_minimal()
 
#cook's distance
plot(acpl.lm, which = 4)


#####################################

niemann$WL <- factor(niemann$WL, levels = c("0","1/2","1"))

niemann.ord <- polr(WL ~ Elo + OppElo + Mean_CP + Std_CP, data=niemann)
summary(niemann.ord)

carlsen.ord <- polr(WL ~ Elo + OppElo + Mean_CP + Std_CP, data=carlsen)
summary(carlsen.ord)


##more eda

ggplot(data = players) + geom_point(aes(x = Elo, y = Mean_CP, color = Name)) + theme_minimal()

ggplot(data = players) + geom_point(aes(x = Elo, y = Mean_CP, color = Time)) + theme_minimal()

#transform data

hist(players$Mean_CP)
