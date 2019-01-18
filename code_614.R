library(xlsx)
data <- read.xlsx("Project_dataset.xlsx", 1, header = FALSE)
head(data, 10)
##first install package "factoextra" and "qicharts"
#"factoextra" is for PCA
#"qicharts" is for control limits

library(factoextra)
library(qicharts2)

##Using correlation matrix
res.pca_cor <- prcomp(data, scale = TRUE)
summary(res.pca_cor)
fviz_eig(res.pca_cor)
res.var.cor <- get_pca_var(res.pca_cor)
res.var.cor$contrib        # Contributions to the PCs

###pca MATRIX USING correlation
PCA_cor <-res.pca_cor$x[,1:10]
dim(PCA_cor)

##Using covariance matrix
res.pca_cov <- prcomp(data, scale = FALSE)
fviz_eig(res.pca_cov)
res.var.cov <- get_pca_var(res.pca_cov)
res.var.cov$contrib

###pca MATRIX USING covariance
PCA_cov <- res.pca_cov$x[,1:3]
dim(PCA_cov)

##
# mfrow = par(c(1,3))
# qic(PCA_cov[,1], chart = 'mr')
# qic(PCA_cov[,2], chart = 'mr')
# qic(PCA_cov[,3], chart = 'mr')
# 
# 
# qic(PCA_cor[,1], chart = 'mr')
# qic(PCA_cor[,2], chart = 'mr')
# qic(PCA_cor[,3], chart = 'mr')
# qic(PCA_cor[,4], chart = 'mr')
# qic(PCA_cor[,5], chart = 'mr')
# qic(PCA_cor[,6], chart = 'mr')
# qic(PCA_cor[,7], chart = 'mr')
# qic(PCA_cor[,8], chart = 'mr')
# qic(PCA_cor[,9], chart = 'mr')
# qic(PCA_cor[,10], chart = 'mr')
# 

data_scaled <- scale(data)

library(MSQC)
library(xlsx)
library(IQCC)
library(MPCI)
library(MASS)
data_scaled <- scale(data)
i <- rep(0,552)

S <- cov(data_scaled)

S_inv <- ginv(S)


#calculating the T2 value

data_mean <- colMeans(data_scaled, na.rm = TRUE)

T2 <- rep(0,552)


##Calculating T2 values for removing outliers
for (i in 1:552)
{
  T2[i] = t(data_scaled[i,] - data_mean) %*% (S_inv) %*% (data_scaled[i,] - data_mean)
}

qchisq(.95, df=209)        # 7 degrees of freedom 
zero <- rep(0,209)


##code for removing outliers
dt <- data.frame()
el <- vector()
for (i in 1:552)
{
  if (T2[i] < qchisq(0.95, 209))
  { dt <-  rbind(dt , data_scaled[i,])
  }
  else
  {
    el <- c(el, i)
  }
}

dim(dt) ##dimension 508
length(el) ##dimension 44
write.csv(el,"outlierrows.csv")



pca_f <- prcomp(dt)
summary(pca_f)
##We can take 10 principal components

E_values <- eigen(cov(dt))

E_vector <- E_values$values

write.csv(E_vector, "eigen_values")


length(E_vector)
pca_data <- pca_f$x
dim(pca_data)

p <- rep(0, nrow(pca_data))
T_m <- rep(0, nrow(pca_data))




##Code for T2(T_m) for multivariate analysis using the PCA values
s <- 0
for (p in (1: nrow(pca_data)))
{
  
  for (q in (1:10))
  {
    s <- (pca_data[p,q])^2/(E_vector[q]) + s
  }

T_m[p] <- s  

s <- 0

}

T_m

qchisq(0.995, 10)
plot(1:508, T_m)
abline(1:508, a)
# plot(1:508, rep(, 508))


line(a)


###Using the T_m values for data deletion
df_1 <- data.frame()
el_1 <- vector()
k <- 0
for (k in 1:508)
{
  if (T_m[k] < qchisq(0.995, 10))
  { df_1 <-  rbind(df_1, dt[k,])
  }
  else
  {
    el_1 <- c(el_1, k)
  }
}

el_1
dim(df_1)

#"[1] 251 415 421 491 493 494" these points are proposed to be removed after the above process



dt_1 <- dt[-el_1,]


###Now doing pca for the second time:


pca_f1 <- prcomp(dt_1)
summary(pca_f1)
##We can take 12 principal components which explain 60% of the data =

E_values1 <- eigen(cov(dt_1))

E_vector1 <- E_values1$values
length(E_vector1)
pca_data1 <- pca_f1$x
dim(pca_data1)



###Loop starts
p1 <- rep(0, nrow(pca_data1))
T_m1 <- rep(0, nrow(pca_data1))
q1 <- rep(0, 209)
s1 <- 0
for (p1 in (1: nrow(pca_data1)))
{
  
  for (q1 in (1:12))
  {
    s1 <- (pca_data1[p1,q1])^2/(E_vector1[q1]) + s1
  }
  
  T_m1[p1] <- s1  
  
  s1 <- 0
  
}
####Loop ends
T_m1; plot(1:502, T_m1); qchisq(0.995, 12) ##T_m1 is the T2 value of the PCA 



####New ITERATION
df_2 <- data.frame()
el_2 <- vector()
l <- 0
for (l in 1:502)
{
  if (T_m1[l] < qchisq(0.995, 12))
  { df_2 <-  rbind(df_2, dt_1[l,])
  }
  else
  {
    el_2 <- c(el_2, l)
  }
}

el_2

##removing data points
dt_2 <- dt_1[-el_2,]

##Applying pca on dt_2

pca_f2 <- prcomp(dt_2)
summary(pca_f2)
##We can take 12 principal components which explain 60% of the data =

E_values2 <- eigen(cov(dt_2))

E_vector2 <- E_values2$values
length(E_vector2)
pca_data2 <- pca_f2$x
dim(pca_data2)

##2 nos data points have been removed
####################

p2 <- rep(0, nrow(pca_data2))
T_m2 <- rep(0, nrow(pca_data2))
q2 <- rep(0, 209)
s2 <- 0
for (p2 in (1: nrow(pca_data2)))
{
  
  for (q2 in (1:12))
  {
    s2 <- (pca_data2[p2,q2])^2/(E_vector1[q2]) + s2
  }
  
  T_m2[p2] <- s2  
  
  s2 <- 0
  
}
####Loop ends
T_m2; plot(1:500, T_m2); qchisq(0.995, 12) ##T_m2 is the T2 value of the PCA for pca_data2 


#####Last removal of data_points

df_3 <- data.frame()
el_3 <- vector()
n <- 0
for (n in 1:500)
{
  if (T_m2[n] < qchisq(0.995, 12))
  { df_3 <-  rbind(df_3, dt_2[n,])
  }
  else
  {
    el_3 <- c(el_3, n)
  }
}
dim(df_3) ##NOW we have 498 points in the data
el_3

##removing data points
dt_3 <- dt_2[-el_3,]


####Applying PCA
pca_f3 <- prcomp(dt_3)
summary(pca_f3)
##We can still take 12 principal components which explain 60% of the data =

E_values3 <- eigen(cov(dt_3))

E_vector3 <- E_values3$values
length(E_vector3)
write.csv(E_vector3,"eigen_vector.csv")
pca_data3 <- pca_f3$x
dim(pca_data3)
#################PCA has been applied
p3 <- rep(0, nrow(pca_data3))
T_m3 <- rep(0, nrow(pca_data3))
q3 <- rep(0, 209)
s3 <- 0
for (p3 in (1: nrow(pca_data3)))
{
  
  for (q3 in (1:12))
  {
    s3 <- (pca_data3[p3,q3])^2/(E_vector1[q3]) + s3
  }
  
  T_m3[p3] <- s3  
  
  s3 <- 0
  
}
####Loop ends
T_m3; plot(1:498, T_m3)
abline(qchisq(0.995, 12), col = "lightgray", lty = 3)
;qchisq(0.995, 12) ##T_m3 is the T2 value of the PCA for pca_data2


write.csv(dt_3,"final_matrix.csv")


data_mean_f <- colMeans(dt_3, na.rm = TRUE)
write.csv(data_mean_f, "Mean_vector_final.csv")


COV <- cov(dt_3)
write.csv(COV, "Covariance_final.csv")

data_FINAL <- data[-el-el_1-el_2-el_3, ]



































