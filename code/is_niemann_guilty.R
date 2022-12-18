# EDA

library(ggplot2)
library(tidyr)

ggplot() + geom_point(data = players, aes(x = Elo, y = Mean_CP), alpha = 0.2) +
  geom_point(data = players[players$Name2=="niemann",], aes(x = Elo, y = Mean_CP, color = Time)) + 
  scale_color_gradient(low = "white", high = "darkred") +
  #scale_x_continuous(limits = c(2400, 2900)) +
  xlab("Elo of Player") + ylab("Mean Centipawn Loss During a Game, w/ Natural Log Transformation") + 
  labs(title = "Does Niemann Have Unusual Centipawn Losses Based on Elo?") + 
  theme_light() + theme(text = element_text(size = 20))

ggplot() + geom_boxplot(data = players, aes(x = Name2, y = Mean_CP, fill = elo.cat)) + 
  xlab("Name of the Player") + ylab("Mean Centipawn Loss During a Game, w/ Natural Log Transformation") + 
  labs(title = "Does Niemann Have Unusual Centipawn Losses Based on Elo?") +
  theme_light()

niemann.long <- players[,c(3,4,10)]
niemann.long <- pivot_longer(niemann.long, cols = 1:2, names_to = "data.type", values_to = "data.values")
colnames(niemann.long) <- c("Name", "data.type", "data.values")

ggplot() + geom_boxplot(data = niemann.long, aes(x = data.type, y = data.values, fill = Name)) + 
  ylab("Mean or Std Centipawn Loss During a Game, with Natural Log Transformation") + 
  labs(title = "Does Niemann Have Unusual Centipawn Losses Based on Elo?") +
  scale_x_discrete(labels = c("Mean Centipawn Loss", "Standard Deviation Centipawn Loss")) + 
  theme_light() + theme(axis.title.x = element_blank()) + theme(text = element_text(size = 18))

##############################################

# regression adding niemann as a term 

## normalization

players$Mean_CP <- log(players$Mean_CP)
players$Std_CP <- log(players$Std_CP)

## adding binary name column

players$Name2 <- ifelse(players$Name=="niemann", "niemann","other")
players$Name2 <- factor(players$Name2, levels = c("other","niemann"))

## optimal model

best.model <- lm(Mean_CP ~ Elo + OppElo + WL, data = players)
summary(best.model)
AIC(best.model)

## model plus name

model.plus.name <- lm(Mean_CP ~ Elo + OppElo + WL + Name2, data = players)
summary(model.plus.name) #Niemann dummy variable not significant
AIC(model.plus.name)

## looking at influential points in the linear model 

###hat/leverage

players$hat <- hatvalues(best.model)
hatthreshold <- 4*2/nrow(players) 

players.hat <- players[players$hat >= hatthreshold,]

ggplot(players.hat) + 
  geom_bar(aes(x = reorder(rownames(players.hat), -hat), y = hat, fill = Name2), stat = "identity") + 
  theme_minimal() + theme(axis.text.x = element_blank()) + 
  xlab("Data Point") + ylab("Hat/Leverage Values") + labs(title = "High Hat Values Using Model without Name Variable")

###cook's distance

players$cookd <- cooks.distance(best.model)
cookdthreshold <- 4/nrow(players)

players.cook <- players[players$cookd >= cookdthreshold,]

ggplot(players.cook) + 
  geom_bar(aes(x = reorder(rownames(players.cook), -cookd), y = cookd, fill = Name2), stat = "identity") + 
  theme_minimal() + theme(axis.text.x = element_blank()) + 
  xlab("Data Point") + ylab("Cook's Distance") + labs(title = "Large Cook's Distances Using Model without Name Variable") + 
  theme(text = element_text(size = 20))

##############################################

# MANOVA and ANOVA

## Checking Assumptions

library(car)
library(mvnormtest)

leveneTest(Mean_CP ~ Name2, data = players)
leveneTest(Std_CP ~ Name2, data = players) #checking for equal variances; assumptions not met

players.sample <- players[sample(nrow(players), 4999), c(1:5)]
mshapiro.test(t(players.sample)) #checking for multivariate normality; assumption not met

players.meancp.shapiro <- tapply(players$Mean_CP, players$Name, shapiro.test)
players.stdcp.shapiro <- tapply(players$Std_CP, players$Name, shapiro.test) #normality across groups with all names; assumption not met

tapply(players$Mean_CP, players$Name2, shapiro.test)
tapply(players$Std_CP, players$Name2, shapiro.test) #normality across groups with binary names; assumption also not met

players$mahal <- mahalanobis(players[,1:6], colMeans(players[,1:6]), cov(players[,1:6])) #checking for multivariate outliers
players$mahal_p <- pchisq(players$mahal, df=6, lower.tail=FALSE) #a lot of outliers, so assumptions technically not met? 
hist(players$mahal_p[players$mahal_p<=0.1], breaks = 100)

## Doing MANOVA anyways
## MANVOA with ACPL and SDCPL for all players

players.manova <- manova(as.matrix(players[,3:4]) ~ players$Name)
summary(players.manova)
summary(players.manova, test = "Wilks")
summary(players.manova, test = "Hotelling-Lawley")
summary(players.manova, test = "Roy")

## MANOVA with ACPL and SDCPL for Niemann vs Rest

players.manova.2 <- manova(as.matrix(players[,3:4]) ~ players$Name2)
summary(players.manova.2)
summary(players.manova.2, test = "Wilks")
summary(players.manova.2, test = "Hotelling-Lawley")
summary(players.manova.2, test = "Roy")

summary.aov(players.manova.2)

## MANOVA with ACPL and SDCPL for Niemann vs Rest for two levels: Elo and Name

#converting elo into three cats
players$elo.cat <- ifelse(players$Elo < 2700, "low",
                                  ifelse(players$Elo >= 2700 & players$Elo < 2775, "med",
                                         ifelse(players$Elo >= 2775, "high", "NA")))
# refactoring cats
players$elo.cat <- factor(players$elo.cat, levels = c("low","med","high"))

#another method for binning elo rating
players <- mutate(players, elo.cat = ntile(Elo, n=10))

players.manova.3 <- manova(as.matrix(players[,3:4]) ~ as.factor(players$elo.cat) * players$Name2) #testing if impact of Elo on responses depends on Name
summary(players.manova.3)
summary(players.manova.3, test = "Wilks")
summary(players.manova.3, test = "Hotelling-Lawley")
summary(players.manova.3, test = "Roy")

summary.aov(players.manova.3)


##############################################

# non-parametric tests

# getting the data again

players.2 <- bind_rows(dat, .id = "column_label")
players.2 <- players.2[,-c(1:2)]

players.2$elo.cat <- ifelse(players.2$Elo < 2700, "low",
                          ifelse(players.2$Elo >= 2700 & players$Elo < 2775, "med",
                                 ifelse(players.2$Elo >= 2775, "high", "NA")))
players.2$elo.cat <- factor(players.2$elo.cat, levels = c("low","med","high"))

players.2$Name2 <- ifelse(players.2$Name=="niemann", "niemann","other")
players.2$Name2 <- factor(players.2$Name2, levels = c("other","niemann"))

players.low.elo <- players.2[players.2$elo.cat=="low",]

#visualizing
ggplot() + geom_point(data = players.low.elo, aes(x = Elo, y = Mean_CP), alpha = 0.2) +
  geom_point(data = players.low.elo[players.low.elo$Name2=="niemann",], aes(x = Elo, y = Mean_CP, color = Time)) + 
  scale_color_gradient(low = "white", high = "darkred") +
  #scale_x_continuous(limits = c(2400, 2900)) +
  xlab("Elo of Player") + ylab("Mean CP during a Game") + 
  labs(title = "Does Niemann Have Unusual Centipawn Losses Based on Elo?") + 
  theme_light() + theme(text = element_text(size = 20))

ggplot() + geom_boxplot(data = players.low.elo, aes(x = Name2, y = Mean_CP, fill = elo.cat)) + 
  xlab("Name of the Player") + ylab("Mean Centipawn Loss During a Game") + 
  labs(title = "Does Niemann Have Unusual Centipawn Losses Based on Elo?") +
  theme_light() + theme(text = element_text(size = 20))


#Wilcoxon Rank-Sum Test
wilcox.test(players.low.elo$Mean_CP ~ players.low.elo$Name2)
kruskal.test(players.low.elo$Mean_CP ~ players.low.elo$Name)




