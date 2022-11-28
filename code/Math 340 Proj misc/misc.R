library(ggplot2)

niemann$WL <- factor(niemann$WL, levels = c("0","1/2","1"))

niemann.ord <- polr(WL ~ Elo + OppElo + Mean_CP + Std_CP, data=niemann)
summary(niemann.ord)

carlsen.ord <- polr(WL ~ Elo + OppElo + Mean_CP + Std_CP, data=carlsen)
summary(carlsen.ord)


##more eda

ggplot(data = players) + geom_point(aes(x = Elo, y = Mean_CP, color = Name)) + theme_minimal()

ggplot(data = players) + geom_point(aes(x = Elo, y = Mean_CP, color = Time)) + theme_minimal()
