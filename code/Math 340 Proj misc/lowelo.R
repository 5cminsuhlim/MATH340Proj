
#plotting

ggplot(data = players.niemann) + geom_point(aes(x = Elo, y = Mean_CP, color = Name)) + 
  theme_minimal()

#getting difference between Elo and OppElo

players$elo.diff <- players$Elo - players$OppElo

summary(lm(Mean_CP ~ Elo + WL + Std_CP + as.factor(Name), data = players.niemann))


