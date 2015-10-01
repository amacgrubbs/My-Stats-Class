## K-Means w/ entropy calculation
library(cluster)
library(fpc)
library(proxy)


yelp_data = (read.csv("C:/Users/Zach/Google Drive/University of Texas/MSBA Fall Classes/Text Analysis/Homework_2/Yelp Data Restaurant Reviews Ratings.csv"))
yelp_data$rating = ifelse(yelp_data$stars > 3,1,0)
yelp_split = yelp_data[1:5000,]

yelp_matrix = create_matrix(yelp_split$Review, language = "english", removeNumbers = TRUE, stemWords = TRUE, removeSparseTerms = .90)
yelp_reviews = as.data.frame(as.matrix(yelp_matrix))

fit = kmeans(yelp_reviews, 2)
clusplot(yelp_reviews, fit$cluster, color=TRUE, shade=TRUE, labels = 0, lines = 0)
plotcluster(yelp_reviews, fit$cluster)


d = dist(yelp_reviews, method = "euclidean")
stats_euclid = cluster.stats(d, fit$cluster)
stats_euclid$entropy

dc = dist(yelp_reviews, method = "cosine")
stats_cosine = cluster.stats(dc, fit$cluster)
stats_cosine$entropy

matrix <- as.matrix(yelp_matrix)
cosineDist <- function(x){
    as.dist(1 - x%*%t(x)/(sqrt(rowSums(x^2) %*% t(rowSums(x^2)))))
}
cosine_dist = cosineDist(matrix)
cosine_dist[is.na(cosine_dist)] <- 0

stats_cosine2 = cluster.stats(cosine_dist, fit$cluster)
stats_cosine2$entropy
