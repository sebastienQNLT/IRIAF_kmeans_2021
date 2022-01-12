library(factoextra)
data("multishapes")
df <- multishapes[, 1:2]
set.seed(123)
km.res <- kmeans(df, 5, nstart = 25)
fviz_cluster(km.res, df, geom = "point")

install.packages("fpc")
install.packages("dbscan")
library(fpc)
library(dbscan)

# Compute DBSCAN using fpc package
set.seed(123)
#determiner la valeur de eps
library(plotly)
dbscan::kNNdistplot(df, k =  5)
abline(h = 0.15, lty = 2)

db <- fpc::dbscan(df, eps = 0.2, MinPts = 5)
# Plot DBSCAN results
fviz_cluster(db, df, stand = FALSE, frame = FALSE,geom = "point")

