## PRINCIPLE COMPONENT ANALYSIS

library(ggplot2)

yelp_data = (read.csv("C:/Users/Zach/Google Drive/University of Texas/MSBA Fall Classes/Text Analysis/Homework_2/Yelp Data Restaurant Reviews Ratings.csv"))
yelp_data$rating = ifelse(yelp_data$stars > 3,1,0)
yelp_split = yelp_data[1:1000,]

yelp_matrix = create_matrix(yelp_split$Review, language = "english", removeNumbers = TRUE, stemWords = TRUE, removeSparseTerms = .90)
yelp_reviews = as.data.frame(as.matrix(yelp_matrix))

centered = scale(yelp_reviews, center=TRUE, scale=FALSE)

pc1 = prcomp(centered, scale=TRUE)

pc1
summary(pc1)
plot(pc1)
biplot(pc1)

loadings = pc1$rotation
scores = pc1$x
qplot(scores[,1], scores[,2], color=yelp_split$rating, xlab='Component 1', ylab='Component 2')
summary(pc1)

## TOP 25 WORDS ASSOCIATED WITH COMPONENT 1 AND COMPONENT 2

order_1 = order(loadings[,1])
colnames(centered)[head(order_1,25)]
colnames(centered)[tail(order_1,25)]

order_2 = order(loadings[,2])
colnames(centered)[head(order_2,25)]
colnames(centered)[tail(order_2,25)]

pve = 100*pc1$sdev^2/sum(pc1$sdev^2)
par(mfrow=c(1,2))
plot(pve, type = "o", ylab = "PVE", xlab = "Principle Component", col = "blue")
plot(cumsum(pve), type = "o", ylab = "Cumulative PVE", xlab = "Principle Component", col = "brown3")


## K MEANS
yelp_500 = yelp_data[1:5000,]
new = as.data.frame(as.matrix(yelp_matrix))
both = cbind(new, yelp_500[,2:20])
both$rating = yelp_500$rating


fit = kmeans(yelp_reviews, 2)
aggregate(yelp_reviews, by=list(fit$cluster), FUN = mean)
mydata = data.frame(yelp_reviews, fit$cluster)
fit1 = kmeans(yelp_reviews, 2)
library(cluster)
clusplot(yelp_reviews, fit$cluster, color=TRUE, shade=TRUE, labels = 0, lines = 0)
library(fpc)
plotcluster(yelp_reviews, fit$cluster)

d = dist(yelp_reviews, method = "euclidean")
clusstats = cluster.stats(d, fit1$cluster)
clusstats$entropy

############

set.seed(35)
km.out = kmeans(yelp_reviews, 2, nstart = 10)
km.out$cluster
km.out$tot.withinss
hist(km.out$cluster, plot = F)

require(useful)
yelp_best = FitKMeans(yelp_reviews, max.clusters = 30, nstart = 20, seed = 35)
yelp_best

require(cluster)
theGap = clusGap(yelp_reviews, FUNcluster = pam, K.max = 20)
gapDF = as.data.frame(theGap$Tab)
gapDF

# logW curves
ggplot(gapDF, aes(x=1:nrow(gapDF))) +
    geom_line(aes(y=logW), color = "blue") +
    geom_point(aes(y=logW), color = "blue") +
    geom_line(aes(y=E.logW), color = "green") +
    geom_point(aes(y=E.logW), color = "green") +
    labs(x = "Number of Clusters")

#gap curve
ggplot(gapDF, aes(x=1:nrow(gapDF)))+ 
    geom_line(aes(y=gap), color = "red") +
    geom_point(aes(y=gap), color = "red") +
    geom_errorbar(aes(ymin=gap-SE.sim, ymax=gap+SE.sim), color = "red") +
    labs(x="Number of Clusters", y = "Gap")
