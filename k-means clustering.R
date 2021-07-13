######### K-means clusttering #########
library(tidyverse)
library(cluster)  ### working with clusters
library(factoextra) ## cal and visualizing clusters
library(gridExtra) ## plotting multiple graphs
library(ggplot2)  ## visualizing

data("USArrests")
df <- USArrests
View(df)
## remove missing value
df <- na.omit(df)
## scale data
df <- scale(df)
head(df)

#cal the distance measure
distance <- get_dist(df)
distance
## viz the distance measure
fviz_dist(distance, gradient = list(low = "#00AFBB", mid = "white",
                                    high = "#FC4E07"))

##### K means clustering
k2 <- kmeans(df, centers = 2, nstart = 25)
str(k2)
k2

### visualize clusters
fviz_cluster(k2, data = df)

## pairwise scatter plots to visualize clusters
df%>%
  as_tibble()%>%
  mutate(cluster = k2$cluster,
         state = row.names(USArrests))%>%
  ggplot(aes(UrbanPop, Murder, color = factor(cluster), label = state)) +
  geom_text()

k3 <- kmeans(df, centers = 3, nstart = 25) ## kmeans with 3 clusters
k4 <- kmeans(df, centers = 4, nstart = 25) ## kmeans with 4 clusters
k5 <- kmeans(df, centers = 5, nstart = 25) ## kmeans with 5 clusters

# plots to compare
p1 <- fviz_cluster(k2, geom = "point", data = df) + ggtitle("k = 2")
p2 <- fviz_cluster(k3, geom = "point", data = df) + ggtitle("k = 3")
p3 <- fviz_cluster(k4, geom = "point", data = df) + ggtitle("k = 4")
p4 <- fviz_cluster(k5, geom = "point", data = df) + ggtitle("k = 5")

############# plot all graphs together  #######
grid.arrange(p1, p2, p3, p4, nrow = 2)


######################## determining optimal clusters (elbow method) procedure #############################
set.seed(123)
## compute total within-cluster sum of square
wss <- function(k){
  kmeans(df, k, nstart = 10)$tot.withinss
}
## compute and plot wss for k = 1 to k = 15
k.values <- 1:15

## extract wss for 2-15 clusters
wss_values <- map_dbl(k.values, wss)

plot(k.values, wss_values,
     type = "b", pch = 19, frame = FALSE,
     xlab = "Number of clusters K",
     ylab = "Total within-clusters sum of square")
## cal elbow method for optimal clusters with fviz_nbclust()
set.seed(123)
fviz_nbclust(df, kmeans, method = "wss")


############# average silhouette computation procedure #####################
avg_sil <- function(k) {
  km.res <- kmeans(df, centers = k, nstart = 25)
  ss <- silhouette(km.res$cluster, dist(df))
  mean(ss[, 3])
}

#### compute and plot wss for k = 2 to k = 15
k.values <- 2:15
## extract avg silhouette for 2-15 clusters
avg_sil_values <- map_dbl(k.values, avg_sil)
## plot values
plot(k.values, avg_sil_values,
     type = "b", pch = 19, frame = FALSE,
     xlab = "Number of clusters K",
     ylab = "Average Silhouettes")
#######computing silhouettes with fviz_nbclust() for optimal clusters
fviz_nbclust(df, kmeans, method = "silhouette")


############## Gap statistics method ##############################
#compute gap statistics
set.seed(123)
gap_stat <- clusGap(df, FUN = kmeans, nstart = 25, K.max = 10, B = 50)
print(gap_stat, method = "firstmax")
fviz_gap_stat(gap_stat)

#######  compute k-means clustering with k = 4
set.seed(123)
final <- kmeans(df, 4, nstart = 25)
print(final)
fviz_cluster(final, data = df)

###extracting clustering and adding to data
USArrests%>%
  mutate(Cluster = final$cluster) %>%
  group_by(Cluster) %>%
  summarise_all("mean")
