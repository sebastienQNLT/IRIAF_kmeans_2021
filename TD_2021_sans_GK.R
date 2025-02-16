library(dplyr)
library(skimr)
library(ggplot2)
library(caret)
library(FactoMineR)
library(factoextra)
library(cluster)
library(colorBrewer)
library(plotly)

set.seed=481516

#setwd("D:/IRIAF/kmeans/TD")

#lecture des donnees ----
fifa_data<-read.csv2("https://raw.githubusercontent.com/sebastienQNLT/IRIAF_kmeans_2021/main/fifa_data.csv",sep=";",stringsAsFactors = FALSE,dec = ".") %>% 
  mutate(sofifa_id=as.character(sofifa_id)) %>% 
  filter(team_position !="GK") %>%
  select(-starts_with("goalkeep")) %>% 
  select(-starts_with("gk")) %>% 
  replace(is.na(.),0)

dim(fifa_data)
fifa_data %>% glimpse

#scaling des donnees----
#on conserve que les donn�es num�riques
fifa_data.scaled<-fifa_data %>% select_if(is.numeric) %>% 
  scale() %>% as.data.frame
fifa_data.scaled %>% glimpse
fifa_data.scaled %>% skim

#ACP----
res.pca <- FactoMineR::PCA(fifa_data.scaled,  graph = FALSE)
(eig.val <- get_eigenvalue(res.pca))

#scree plot
fviz_screeplot(res.pca, addlabels = TRUE, ylim = c(0, 70))
# ? axes

# graph des contributions des  variables
# Contributions of variables to PCx
fviz_contrib(res.pca, choice = "var", axes = 1, top = 10)
fviz_contrib(res.pca, choice = "var", axes = 2, top = 10)

#visualisation des axes 1 et 2
fviz_pca_var(res.pca, col.var = "contrib", 
             ggtheme = theme_minimal(),
             select.var = list(contrib = 15)
)
#on v�rifie a partir des coordonn�es des variables
(coord.var.pca<-res.pca$var$coord %>% as.data.frame())

#graph des individus
#axe 1 et 2
fviz_pca_ind(res.pca, geom = "point",alpha.ind=.1) +theme_minimal()

ncp.retenu=5

#kmeans ----

res.pca <- FactoMineR::PCA(fifa_data.scaled,  graph = FALSE, ncp = ncp.retenu)
df.kmeans<-as.data.frame(res.pca$ind$coord) 

#on teste avec k = 3
clusters <- kmeans(df.kmeans, 3, nstart = 50)
clusters$tot.withinss # variance intra
clusters$size #taille des clusters

# graph des individus, en fonction des dimensions 1 et 2 de l'ACP
fviz_cluster(clusters, data = df.kmeans,geom="point")


#elbow----
#on cr�� une fonction qui retourne la variance intra pour une valeur de k
kmean_withinss <- function(k) {
  print(paste0("kmeans k:",k))
  cluster <- kmeans(df.kmeans, k, nstart = 25)
  return (cluster$tot.withinss)
}
# Set maximum cluster 
max_k <-15
# Run algorithm over a range of k 
wss <- sapply(2:max_k, kmean_withinss)
# Create a data frame to plot the graph
elbow <-data.frame(2:max_k, wss)
# Plot the graph with gglop
ggplot(elbow, aes(x = X2.max_k, y = wss)) +
  geom_point() +
  geom_line() +
  scale_x_continuous(breaks = seq(1, max_k, by = 1))

#--> ? clusters

#silhouette----
# function to compute average silhouette for k clusters
avg_sil <- function(k) {
  print(paste0("silhouette k:",k))
  km.res <- kmeans(df.kmeans, centers = k, nstart = 25)
  ss <- silhouette(km.res$cluster, dist(df.kmeans))
  return(mean(ss[, 3]))
}

# Set maximum cluster 
max_k <-15
# Run algorithm over a range of k 
avg.silhouette <- sapply(2:max_k, avg_sil)
# Create a data frame to plot the graph
silhouette <-data.frame(2:max_k, avg.silhouette)
# Plot the graph
ggplot(silhouette, aes(x = X2.max_k, y = avg.silhouette)) +
  geom_point() +
  geom_line() +
  scale_x_continuous(breaks = seq(1, max_k, by = 1))


k.retenu=6

# kmeans 8 clusters ----
cluster.2 <- kmeans(df.kmeans, k.retenu, nstart = 25)
cluster.2$size
# graph des individus, en fonction des dimensions 1 et 2 de l'ACP
fviz_cluster(cluster.2, data = df.kmeans,geom="point")

s<-silhouette(cluster.2$cluster, dist(df.kmeans))
fviz_silhouette(s)

# ajout du r�sultat du clustering � la base initiale
# calcul des moyennes des variables et non pas des coordonn�es sur les axes de l'ACP
player.clustered <-fifa_data %>%
  mutate(cluster = cluster.2$cluster)

stats<-player.clustered %>% 
  group_by(cluster) %>%
  summarise_all("mean")
stats %>% glimpse

stats.scaled<-stats %>% select(-cluster) %>% scale %>% as.data.frame %>% cbind(stats$cluster)
stats.reshaped<-reshape2::melt(stats.scaled, id.vars=c("stats$cluster"))
ggplot(data = stats.reshaped, aes(x = `stats$cluster`, y =variable, fill = value)) +
  scale_x_continuous(breaks = seq(1, 8, by = 1)) +
  geom_tile() +
  coord_equal() +
  scale_fill_gradientn(colours = topo.colors(100))+
  theme_classic()

# verification de notre clustering avec la position reelle des joueurs
p <-  ggplot(player.clustered, aes(team_position))  + geom_bar() +  facet_grid(. ~ cluster)
ggplotly(p)
