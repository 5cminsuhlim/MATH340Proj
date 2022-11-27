## means
carlsen_mean <- read.csv('../output/mean_cp_loss/carlsen.csv')
erigaisi_mean <- read.csv('../output/mean_cp_loss/erigaisi.csv')
gukesh_mean <- read.csv('../output/mean_cp_loss/gukesh.csv')
nepo_mean <- read.csv('../output/mean_cp_loss/nepo.csv')
niemann_mean <- read.csv('../output/mean_cp_loss/niemann.csv')

## not normally distributed (right-skewed)
hist(carlsen_mean$X0)
hist(erigaisi_mean$X0)
hist(gukesh_mean$X0)
hist(nepo_mean$X0)
hist(niemann_mean$X0)

boxplot(carlsen_mean$X0)
boxplot(erigaisi_mean$X0)
boxplot(gukesh_mean$X0)
boxplot(nepo_mean$X0)
boxplot(niemann_mean$X0)


## std
carlsen_std <- read.csv('../output/std_cp_loss/carlsen.csv')
erigaisi_std <- read.csv('../output/std_cp_loss/erigaisi.csv')
gukesh_std <- read.csv('../output/std_cp_loss/gukesh.csv')
nepo_std <- read.csv('../output/std_cp_loss/nepo.csv')
niemann_std <- read.csv('../output/std_cp_loss/niemann.csv')

## not normally distributed (right-skewed)
hist(carlsen_std$X0)
hist(erigaisi_std$X0)
hist(gukesh_std$X0)
hist(nepo_std$X0)
hist(niemann_std$X0)

boxplot(carlsen_std$X0)
boxplot(erigaisi_std$X0)
boxplot(gukesh_std$X0)
boxplot(nepo_std$X0)
boxplot(niemann_std$X0)
