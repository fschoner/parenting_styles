"
This script reproduces the analysis in 
http://statmath.wu.ac.at/courses/multverf2/tutorien/PCA.pdf
which should help me get started with PCA.

"



library(haven)
library(tidyverse)
library(data.table)


# Paths for datasets.
path_in_data <- "src/original_data/"

load(str_c(path_in_data, "GSA.rda"))



gsa <- GSA[, c(5, 10:12, 14, 25:27, 29)]
gsa <- na.omit(gsa)
sucrate <- function(x) prop.table(table(x))[2]
gsa <- aggregate(gsa, list(gsa$country), sucrate)
rownames(gsa) <- gsa[, 1]
gsa <- gsa[, -(1:2)]
     
# if scale = FALSE (default), then covariances are used. In this case, take
# scaled covariances, that is, correlations :-), which is always recommended.
gsa.pca <- prcomp(gsa, scale = TRUE)
gsa.pca

# This gives you SDs (of the components?) and loadings.

# variances of components
gsa.var <- gsa.pca$sdev^2
# proportion of variance explained
gsa.var/sum(gsa.var)
# cumulative
cumsum(gsa.var)/sum(gsa.var)


# Plots
plot(gsa.pca)
biplot(gsa.pca)

#biplot: shows that the x-direction is used to differentiate sports vs. cultural
# activities and 

# how do we get the principal component scores for each row in the data?
gsa.pred <- predict(gsa.pca)
gsa.pred

# Further Plots
plot(gsa.pred[, 1:2])
plot(gsa.pred[, 1:2], type = "n", xlab = "PC1", ylab = "PC2")
text(gsa.pred[, 1:2], rownames(gsa))


# Let's do sth. similar: clustering
# euclidean norm b/c data is numeric
gsa.dist <- dist(scale(gsa), method = "euclidean")

# try out other methods as well.
gsa.hclust <- hclust(gsa.dist, method = "average")
summary(gsa.hclust)
gsa.hclust


plot(gsa.hclust)
# looks like 3 clusters, but choice is essentially arbitrary
rect.hclust(gsa.hclust, k = 3)

# classification is arbitrary here
gsa3 <- cutree(gsa.hclust, k = 3)
gsa3

# use variables from the dataset
plot(gsa$SA02.cycle ~ gsa3)
plot(gsa$SA19.sight ~ gsa3)
# first cluster likes to do sports, third cultural activities

# further tutorials on hierarchical clustering and k-means
