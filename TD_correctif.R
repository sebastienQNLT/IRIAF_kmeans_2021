setwd("D:/IRIAF/M1_KMEANS/TD/2021_partie2")

#https://www.kaggle.com/arjunbhasin2013/ccdata

library(dplyr)
library(skimr)
library(corrplot)
library(cluster)

credit_card<-read.csv2("https://raw.githubusercontent.com/sebastienQNLT/IRIAF_kmeans_2021/main/CC%20GENERAL.csv",sep=",",stringsAsFactors = FALSE,dec = ".")
credit_card %>% skim

credit_card_2<-credit_card %>% select_if(is.numeric) %>% na.omit() %>% scale()


# alternative valeurs manquantes
#knn imputation
# library(caret)
# 
# preProcValues <- preProcess(credit_card %>% 
#                               dplyr::select( BALANCE,BALANCE_FREQUENCY,              
#                                              PURCHASES,ONEOFF_PURCHASES,                
#                                              INSTALLMENTS_PURCHASES,CASH_ADVANCE,                    
#                                              PURCHASES_FREQUENCY,ONEOFF_PURCHASES_FREQUENCY,      
#                                              PURCHASES_INSTALLMENTS_FREQUENCY,CASH_ADVANCE_FREQUENCY,
#                                              CASH_ADVANCE_TRX,PURCHASES_TRX,                   
#                                              CREDIT_LIMIT,PAYMENTS,                        
#                                              MINIMUM_PAYMENTS,PRC_FULL_PAYMENT,                
#                                              TENURE),
#                             method = c("knnImpute"),
#                             k = 5,
#                             knnSummary = mean)
# impute_credit_card <- predict(preProcValues, credit_card,na.action = na.pass)
# impute_credit_card %>% skim
# 
# nearZeroVar(impute_credit_card,names = TRUE)
# credit_card_2<-impute_credit_card %>% select(-TENURE,-CUST_ID)
# 
# X<-cor(credit_card_2)
# findCorrelation(X,names = TRUE)
# credit_card_3<-credit_card_2 %>% select(-PURCHASES)

#PCA----
library(FactoMineR)
library(factoextra)

res.pca <- FactoMineR::PCA(credit_card_2,  graph = FALSE)
(eig.val <- get_eigenvalue(res.pca))
fviz_screeplot(res.pca, addlabels = TRUE, ylim = c(0, 70))
#combien d'axes on conserve ? 3

# graph des contributions des  variables
# Contributions of variables to PCx
fviz_contrib(res.pca, choice = "var", axes = 1, top = 10)
fviz_contrib(res.pca, choice = "var", axes = 2, top = 10)
fviz_contrib(res.pca, choice = "var", axes = 3, top = 10)

#visualisation des axes 1 et 2
fviz_pca_var(res.pca, col.var = "contrib", 
             ggtheme = theme_minimal(),
             select.var = list(contrib = 15)
)

#on vérifie a partir des coordonnées des variables
(coord.var.pca<-res.pca$var$coord %>% as.data.frame()) %>% arrange(Dim.1)
(coord.var.pca<-res.pca$var$coord %>% as.data.frame()) %>% arrange(Dim.2)

#graph des individus
#axe 1 et 2
fviz_pca_ind(res.pca, geom = "point",alpha.ind=.1) +theme_minimal()
fviz_pca_ind(res.pca, geom = "point",alpha.ind=.1,axes = c(1, 3)) +theme_minimal()
fviz_pca_ind(res.pca, geom = "point",alpha.ind=.1,axes = c(2, 3)) +theme_minimal()

#kmeans----
ncp.retenu=6

res.pca <- FactoMineR::PCA(credit_card_2,  graph = FALSE, ncp = ncp.retenu)
df.kmeans<-as.data.frame(res.pca$ind$coord) 

#silhouette----
# function to compute average silhouette for k clusters
avg_sil <- function(k) {
  print(paste0("silhouette k:",k))
  km.res <- kmeans(df.kmeans, centers = k, nstart = 25)
  ss <- silhouette(km.res$cluster, dist(df.kmeans))
  return(mean(ss[, 3]))
}

# Set maximum cluster 
max_k <-10
# Run algorithm over a range of k 
avg.silhouette <- sapply(2:max_k, avg_sil)
# Create a data frame to plot the graph
silhouette <-data.frame(2:max_k, avg.silhouette)
# Plot the graph
library(ggplot2)
ggplot(silhouette, aes(x = X2.max_k, y = avg.silhouette)) +
  geom_point() +
  geom_line() +
  scale_x_continuous(breaks = seq(1, max_k, by = 1))

# 
clusters <- kmeans(df.kmeans, 3, nstart = 50)
fviz_cluster(clusters, data = df.kmeans,geom="point")
fviz_cluster(clusters, data = df.kmeans,geom="point",axes = c(1, 3))
fviz_cluster(clusters, data = df.kmeans,geom="point",axes = c(2, 3))

s<-silhouette(clusters$cluster, dist(df.kmeans))
fviz_silhouette(s)


#CAH
library(cluster)    # clustering algorithms
hc2 <- agnes(df.kmeans, method = "ward")
