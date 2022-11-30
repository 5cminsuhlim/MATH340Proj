#ANOVA & MANOVA

##Data Processing

players.2 <- filter(players, players$Elo != 0) #filtering out players with 0 elo
players.2 <- players.2[,c(4:9, 11)]

players.niemann <- players.2
players.niemann$Name <- ifelse(players.niemann$Name=="niemann", "niemann","other") #dataset to compare niemann vs others

##Checking Assumptions

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

players.niemann$elo.cat <- ifelse(players.niemann$Elo < 2500, "low",
                                  ifelse(players.niemann$Elo >= 2500 & players.niemann$Elo < 2750, "med",
                                        ifelse(players.niemann$Elo >= 2750, "high", "NA")))


###MANOVA with ACPL and SDCPL with multiple IVs, including categorical elo

players.niemann.manova.twoway.elo <- manova(as.matrix(players.niemann[,2:3]) ~ players.niemann$Name * as.factor(players.niemann$elo.cat))
summary(players.niemann.manova.twoway.elo) # testing for interaction

players.niemann.manova.twoway.elo <- manova(as.matrix(players.niemann[,2:3]) ~ players.niemann$Name + as.factor(players.niemann$elo.cat))
summary(players.niemann.manova.twoway.elo) 

summary.aov(players.niemann.manova.twoway.elo)

###Some accompanying visualization

ggplot() + geom_point(data = players.niemann[players.niemann$Name=="other",], aes(x = Mean_CP, y = Std_CP), color = "gray", alpha = 0.2) +
  geom_point(data = players.niemann[players.niemann$Name=="niemann",], aes(x = Mean_CP, y = Std_CP, color = elo.cat)) + 
  #scale_color_gradient(low = "white", high = "darkred") +
  theme_minimal()

#########################

#Influence Measures

players.lm <- lm(Std_CP ~ Elo + Time + WL, data = players.2)
summary(players.lm)

influence.players.lm <- influence.measures(players.lm)
plot(players.lm, which = 4)

#########################

#creating a dataset that merges games by player-year

playeryear <- summarize(group_by(players, Name, Time),
                        mean_acpl = mean(Mean_CP),
                        mean_sdcpl = mean(Std_CP),
                        win_ratio = mean(WL),
                        Elo = mean(Elo))

########################

library(ggplot2)

ggplot(data = playeryear[playeryear$Elo != 0,]) + geom_point(aes(x = Elo, y = mean_sdcpl, color = mean_acpl)) +
  theme_minimal() + scale_color_gradient(low = "lightblue", high = "darkred")

#sd weighted least squares

sd.lm <- lm(mean_sdcpl ~ Elo + win_ratio, data = playeryear[playeryear$Elo != 0,])
summary(sd.lm)

plot(playeryear[playeryear$Elo != 0,]$win_ratio, abs(sd.lm$resid))







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
