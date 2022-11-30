
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
