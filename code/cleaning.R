## MAKE SURE TO SETWD TO INSIDE CODE FOLDER

## only over-the-board games
## age
## how long they've been playing chess
## mean_cp_less = beta_0 + beta_1 * age + beta_2 * time + beta_3 * elo
## std_cp_less = beta_0 + beta_1 * age + beta_2 * time + beta_3 * elo

## means
carlsen_mean <- read.csv('../output/mean_cp_loss/carlsen.csv')
erigaisi_mean <- read.csv('../output/mean_cp_loss/erigaisi.csv')
gukesh_mean <- read.csv('../output/mean_cp_loss/gukesh.csv')
nepo_mean <- read.csv('../output/mean_cp_loss/nepo.csv')
niemann_mean <- read.csv('../output/mean_cp_loss/niemann.csv')

## remove NAs
carlsen_mean <- carlsen_mean[complete.cases(carlsen_mean), ]
erigaisi_mean <- erigaisi_mean[complete.cases(erigaisi_mean), ]
gukesh_mean <- gukesh_mean[complete.cases(gukesh_mean), ]
nepo_mean <- nepo_mean[complete.cases(nepo_mean), ]
niemann_mean <- niemann_mean[complete.cases(niemann_mean), ]


## std
carlsen_std <- read.csv('../output/std_cp_loss/carlsen.csv')
erigaisi_std <- read.csv('../output/std_cp_loss/erigaisi.csv')
gukesh_std <- read.csv('../output/std_cp_loss/gukesh.csv')
nepo_std <- read.csv('../output/std_cp_loss/nepo.csv')
niemann_std <- read.csv('../output/std_cp_loss/niemann.csv')

## remove NAs
carlsen_std <- carlsen_std[complete.cases(carlsen_std), ]
erigaisi_std <- erigaisi_std[complete.cases(erigaisi_std), ]
gukesh_std <- gukesh_std[complete.cases(gukesh_std), ]
nepo_std <- nepo_std[complete.cases(nepo_std), ]
niemann_std <- niemann_std[complete.cases(niemann_std), ]


## elo
carlsen_elo <- read.csv('../output/elo/carlsen.csv')
erigaisi_elo <- read.csv('../output/elo/erigaisi.csv')
gukesh_elo <- read.csv('../output/elo/gukesh.csv')
nepo_elo <- read.csv('../output/elo/nepo.csv')
niemann_elo <- read.csv('../output/elo/niemann.csv')

## keep only general Elo column
carlsen_elo <- subset(carlsen_elo, select="Elo")
erigaisi_elo <- subset(erigaisi_elo, select="Elo")
gukesh_elo <- subset(gukesh_elo, select="Elo")
nepo_elo <- subset(nepo_elo, select="Elo")
niemann_elo <- subset(niemann_elo, select="Elo")

## remove empty rows
erigaisi_elo <- subset(erigaisi_elo, Elo!=0)
gukesh_elo <- subset(gukesh_elo, Elo!=0)
niemann_elo <- subset(niemann_elo, Elo!=0)

## remove NAs
carlsen_elo <- na.omit(carlsen_elo)
erigaisi_elo <- na.omit(erigaisi_elo)
gukesh_elo <- na.omit(gukesh_elo)
nepo_elo <- na.omit(nepo_elo)
niemann_elo <- na.omit(niemann_elo)


## age
carlsen_age <- data.frame(Age=c(rep(29,26), rep(30,59), rep(31,36)))
erigaisi_age <- data.frame(Age=c(rep(16,26), rep(17,70), rep(18,181)))
gukesh_age <- data.frame(Age=c(rep(13,27), rep(14,27), rep(15,179), rep(16,44)))
nepo_age <- data.frame(Age=c(rep(29,7), rep(30,18), rep(31,44), rep(32,9)))
niemann_age <- data.frame(Age=c(rep(15,8), rep(16,152), rep(17,172), rep(18,28)))


## time spent playing
carlsen_time <- data.frame(Time=c(rep(21,26), rep(22,59), rep(23,36))) #https://www.britannica.com/biography/Magnus-Carlsen#:~:text=Carlsen's%20father%20first%20taught%20him,at%20the%20age%20of%20eight.
erigaisi_time <- data.frame(Time=c(rep(9,26), rep(10,70), rep(11,244))) #https://www.tepesigemanchess.com/erigaisi/
gukesh_time <- data.frame(Time=c(rep(6,27), rep(7,27), rep(8,179), rep(9,44))) #https://en.wikipedia.org/wiki/Gukesh_D#:~:text=Gukesh%20was%20born%20on%2029,at%20the%20age%20of%20seven.
nepo_time <- data.frame(Time=c(rep(25,7), rep(26,18), rep(27,44), rep(28,9))) #https://www.chess.com/players/ian-nepomniachtchi#:~:text=He%20played%20chess%20from%20a,he%20outrated%20at%20that%20point.
niemann_time <- data.frame(Time=c(rep(7,8), rep(8,152), rep(9,172), rep(10,28))) #https://www.uschesschamps.com/bio/hans-niemann-0#:~:text=Bio%3A,moved%20to%20Connecticut%20in%202015.


## merge dfs
carlsen_elo <- head(carlsen_elo, 121)
carlsen <- data.frame(Age=carlsen_age$Age, Elo=carlsen_elo$Elo, Mean_CP=carlsen_mean$X0, Std_CP=carlsen_std$X0)

erigaisi_elo <- head(erigaisi_elo, 277)
erigaisi <- data.frame(Age=erigaisi_age$Age, Elo=erigaisi_elo$Elo, Mean_CP=erigaisi_mean$X0, Std_CP=erigaisi_std$X0)

gukesh_mean <- head(gukesh_mean, 277)
gukesh_std <- head(gukesh_std, 277)
gukesh <- data.frame(Age=gukesh_age$Age, Elo=gukesh_elo$Elo, Mean_CP=gukesh_mean$X0, Std_CP=gukesh_std$X0)

nepo <- data.frame(Age=nepo_age$Age, Elo=nepo_elo$Elo, Mean_CP=nepo_mean$X0, Std_CP=nepo_std$X0)

niemann_mean <- head(niemann_mean, 360)
niemann_std <- head(niemann_std, 360)
niemann <- data.frame(Age=niemann_age$Age, Elo=niemann_elo$Elo, Mean_CP=niemann_mean$X0, Std_CP=niemann_std$X0)


## export
write.csv(carlsen, '../data/carlsen.csv', row.names=F)
write.csv(erigaisi, '../data/erigaisi.csv', row.names=F)
write.csv(gukesh, '../data/gukesh.csv', row.names=F)
write.csv(nepo, '../data/nepo.csv', row.names=F)
write.csv(niemann, '../data/niemann.csv', row.names=F)
