##### Principal Component Analysis

library(dplyr)
library(ggplot2)
library(tidyverse)
library(gridExtra)

data("USArrests")
head(USArrests, 10)
############### Procedure fo computing Principal Component Analysis (PCA)  ######################
### compute variance of each variable
apply(USArrests, 2, var)

##### 1.standardizing each variable 
## create dataframe with centered variable
scaled_df <- apply(USArrests, 2, scale)
head(scaled_df)

## 2. cal eigenvalues & eigenvectors
arrest.cov <- cov(scaled_df)
arrests.eigen <- eigen(arrest.cov)
str(arrests.eigen)

### 3. extract the principal component loadings
(phi <- arrests.eigen$vectors[,1:2])

#### 3. transform PCA towards positive direction
phi <- -phi
row.names(phi) <- c("Murder", "Assault", "UrbanPop", "Rape")
colnames(phi) <- c("PC1", "PC2")
phi

### 4. cal Principal Components scores
PC1 <- as.matrix(scaled_df) %*% phi[,1]
PC2 <- as.matrix(scaled_df) %*% phi[,2]
## create data frame with Principal Components scores
PC <- data.frame(State = row.names(USArrests), PC1, PC2)
head(PC)

## Plot Principal components for each State
ggplot(PC, aes(PC1, PC2)) + modelr::geom_ref_line(h = 0) +
  modelr::geom_ref_line(v = 0) +
  geom_text(aes(label = State), size = 3) +
  xlab("First Principal Component") +
  ylab("Second Principal Component") +
  ggtitle("First Two Principal Components of USArrests Data")

##### 6. Proportion of variance explained (PVE)
PVE <- arrests.eigen$values / sum(arrests.eigen$values)
PVE

## PVE (aka scree) plot
PVEplot <- qplot(c(1:4), PVE) +
  geom_line() +
  xlab("Principal Component") +
  ylab("PVE") +
  ggtitle("Scree Plot") +
  ylim(0, 1)

# cumulative PVE plot
cumPVE <- qplot(c(1:4), cumsum(PVE)) +
  geom_line() +
  xlab("Principal Component") +
  ylab("NULL") +
  ggtitle("Cumulative Scree Plot") +
  ylim(0,1)

grid.arrange(PVEplot, cumPVE, ncol = 2)

################### PCA with prcomp()  ###############################
##### prcomp() cal some statistics for use in PCA
pca_results <- prcomp(USArrests, scale = TRUE)
names(pca_results)
pca_results$rotation  ## pricinpal components loadings
pca_results$rotation <- -pca_results$rotation
pca_results$rotation

# PCA scores
pca_results$x <- -pca_results$x
head(pca_results$x)
## biplot
biplot(pca_results, scale = 0)

## standard deviaton of result
pca_results$sdev
# Variance Explained by each principal component
(VE <- pca_results$sdev^2)
VE
## Proportion of variance explained
PVA <- VE/sum(VE)
round(PVE, 2)
