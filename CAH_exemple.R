library(dplyr)  # data manipulation
library(cluster)    # clustering algorithms
library(factoextra) # clustering visualization
library(skimr)
library(purrr)

#lecture du dataset // suppression des valeurs manquantes // scaling
df <- USArrests %>% na.omit() %>% scale()

df %>% skim

#AGNES ----
# Compute with agnes
hc2 <- agnes(df, method = "complete")

# Agglomerative coefficient
hc2$ac
#plus le coefficient est proche de 1 meilleur est le clustering

#comparaison des methodes
# methods to assess
m <- c( "average", "single", "complete", "ward")
names(m) <- c( "average", "single", "complete", "ward")

# function to compute coefficient
ac <- function(x) {
  agnes(df, method = x)$ac
}

map_dbl(m, ac)
##   average    single  complete      ward 
## 0.7379371 0.6276128 0.8531583 0.9346210

hc3 <- agnes(df, method = "ward")
pltree(hc3, cex = 0.6, hang = -1, main = "Dendrogram of agnes") 

#DIANA ----
# compute divisive hierarchical clustering
hc4 <- diana(df)
# Divise coefficient; amount of clustering structure found
hc4$dc
## [1] 0.8514345
# plot dendrogram
pltree(hc4, cex = 0.6, hang = -1, main = "Dendrogram of diana")

#choix du nombre de clusters ----
fviz_nbclust(df, FUN = hcut, method = "wss")
fviz_nbclust(df, FUN = hcut, method = "silhouette")

hc5<-agnes(df, method = "ward")
pltree(hc5, cex = 0.6)
rect.hclust(hc5, k = 3, border = 2:5)

sub_grp <- cutree(hc5, k = 3)
fviz_cluster(list(data = df, cluster = sub_grp))
