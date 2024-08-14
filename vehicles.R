install.packages("readxl")
library(readxl)
file_path <- "D:/UOW/L5/ML_CW/vehicles.xlsx"
df <- read_excel(file_path)
df
library(tidyr)
library(ggplot2)
df_long <- gather(df[2:19], key = "feature", value = "value")
ggplot(df_long, aes(x = feature, y = value)) +
  geom_boxplot() +
  labs(x = "Feature", y = "Value") +
  ggtitle("Boxplot of 18 Features")
dfNormZ <- as.data.frame( scale(df[2:19] ))
dfNormZ
dfNormZ_long <- gather(dfNormZ, key = "feature", value = "value")
ggplot(dfNormZ_long, aes(x = feature, y = value)) +
geom_boxplot() +
  labs(x = "Feature", y = "Value") +
  ggtitle("Boxplot of 18 Features")
remove_outliers <- function(data, k = 1.5) {
  Q1 <- apply(data, 2, quantile, probs = 0.25)
  Q3 <- apply(data, 2, quantile, probs = 0.75)
  IQR <- Q3 - Q1
  lower_bound <- Q1 - k * IQR
  upper_bound <- Q3 + k * IQR
  return(data[apply(data, 1, function(x) all(x >= lower_bound & x <= upper_bound)), ])
}
dfNoOutliers <- remove_outliers(dfNormZ)
dfNoOutliers
dfNoOutliers_long <- gather(dfNoOutliers, key = "feature", value = "value")
ggplot(dfNoOutliers_long, aes(x = feature, y = value)) +
  geom_boxplot() +
  labs(x = "Feature", y = "Value") +
  ggtitle("Boxplot of 18 Features")

install.packages("NbClust")
library(NbClust)
nb <- NbClust(data = dfNoOutliers, distance = "euclidean", min.nc = 2, max.nc = 5, method = "kmeans")
plot(nb)

library(factoextra)
elbow <- fviz_nbclust(dfNoOutliers, kmeans, method = "wss")
plot(elbow, frame = FALSE, xlab="Number of Clusters (k)", ylab="Within-cluster Sum of Squares (WSS)")

library(cluster)
gapstat <- fviz_nbclust(dfNoOutliers, kmeans, method="gap_stat")
plot(gapstat)

silhouette <- fviz_nbclust(dfNoOutliers, kmeans, method = "silhouette", k.max = 10)
plot(silhouette, frame = FALSE,  xlab = "Number of clusters K", ylab = "Average Silhouettes")

kc <- kmeans(dfNoOutliers,2)
kc
fviz_cluster(kc, data = dfNoOutliers)
center <- kc$centers
center
bss = kc$betweenss
bss
tss = kc$totss
tss
bss_tss_ratio <- bss / tss
bss_tss_ratio
wss = kc$tot.withinss
wss
sil <- silhouette(kc$cluster, dist(dfNoOutliers))
fviz_silhouette(sil)
pca_ch_idx <- cluster.stats(dist(dfNoOutliers), kc$cluster)$ch
print(paste("Calinski-Harabasz Index:", pca_ch_idx))

vehicle.cov <- cov(dfNoOutliers)
vehicle.eigen <- eigen(vehicle.cov)
str(vehicle.eigen)

dim(vehicle.eigen$vectors)
ncol(vehicle.eigen$vectors)
phi <- vehicle.eigen$vectors[, 1:18]
row.names(phi) <- c("Comp", "Circ", "D.Circ", "Rad.Ra","Pr.Axis.Ra", "Max.L.Ra", "Scat.Ra", "Elong","Pr.Axis.Rect", "Max.L.Rect", "Sc.Var.Maxis", "Sc.Var.maxis", "Ra.Gyr", "Skew.Maxis", "Skew.maxis", "Kurt.maxis","Kurt.Maxis","Holl.Ra")
colnames(phi) <- c("PC1", "PC2","PC3", "PC4","PC5", "PC6","PC7", "PC8", "PC9", "PC10", "PC11", "PC12", "PC13", "PC14", "PC15", "PC16", "PC17", "PC18")
phi

pca_result <- prcomp(dfNoOutliers, scale. = TRUE)
summary_pca <- summary(pca_result)
VE <- summary_pca$sdev^2 / sum(summary_pca$sdev^2)
cumulative_score <- cumsum(VE)
print(cumulative_score)

vehicle_transform = as.data.frame(pca_result$x[,1:6])
nb2 <- NbClust(data = vehicle_transform, distance = "euclidean", min.nc = 2, max.nc = 10, method = "kmeans")
plot(nb2)

elbow2 <- fviz_nbclust(vehicle_transform, kmeans, method = "wss")
plot(elbow2, frame = FALSE, xlab="Number of Clusters (k)", ylab="Within-cluster Sum of Squares (WSS)")
gapstat2 <- fviz_nbclust(vehicle_transform, kmeans, method="gap_stat")
plot(gapstat2)
silhouette2 <- fviz_nbclust(vehicle_transform, kmeans, method = "silhouette", k.max = 10)
plot(silhouette2, frame = FALSE,  xlab = "Number of clusters K", ylab = "Average Silhouettes")
kc2 <- kmeans(vehicle_transform,2)
kc2
fviz_cluster(kc2, data = vehicle_transform)
center2 <- kc2$centers
center2
bss2 = kc2$betweenss
bss2
tss2 = kc2$totss
tss2
bss_tss_ratio2 <- bss2 / tss2
bss_tss_ratio2
bss_tss_ratio2
wss2 = kc2$tot.withinss
wss2
sil2 <- silhouette(kc2$cluster, dist(vehicle_transform))
fviz_silhouette(sil2)

install.packages("fpc")
library(fpc)
pca_ch_idx2 <- cluster.stats(dist(vehicle_transform), kc2$cluster)$ch
print(paste("Calinski-Harabasz Index:", pca_ch_idx2))
