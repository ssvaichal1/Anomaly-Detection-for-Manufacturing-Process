library(xlsx)
dat <- read.xlsx("Project_dataset.xlsx", 1, header = FALSE)
str(dat)

##refer to this again in the future
hist(dat[,209])

n = nrow(dat)
n
i=0

scaled.dat <- scale(dat)


##plotting the histogram AGAIN after rescaling
hist(scaled.dat[,209])
hist(scaled.dat[,100])

# PCA below:
# Use Ctrl + Shift + C to comment out selected section in R studio in Windows, use the same for reversing
# library(stats)
# pca1 = prcomp(scaled.dat, scale. = FALSE)
# pca1$sdev
# head(pca1$rotation)
# summary(pca1)


cov1 <- cov(scaled.dat)

dim(cov1)

library(MASS)
inv1 <- ginv(scaled.dat)
summary(inv1)

T <- scaled.dat %*% cov1 %*% inv1

str(T)




