library(cluster)
library(fpc)
library(scatterplot3d)
library(tm)

yelp_full = read.csv("homeworks/hw2/Yelp\ Data\ Restaurant\ Reviews\ Ratings.csv")
yelp_full$rating = as.numeric(yelp_full$stars > 3)
numerics = c("votes_cool", "votes_funny", "votes_useful", "Cheap", "Moderate", "Expensive",
             "VeryExpensive", "American", "Chinese", "French", "Japanese", "Indian", 
             "Italian", "Greek", "Mediterranean", "Mexican", "Thai", "Vietnamese", "Others")
yelp_text = yelp_full[c( "Review")]
yelp_targets = yelp_full[c("stars", "rating")]
yelp_corp = Corpus(DataframeSource(yelp_text))

yelp_corp = tm_map(yelp_corp, content_transformer(removeWords), stopwords(kind="SMART"))
yelp_corp = tm_map(yelp_corp, content_transformer(stripWhitespace))
yelp_corp = tm_map(yelp_corp, content_transformer(tolower))
yelp_corp = tm_map(yelp_corp, content_transformer(removePunctuation))
yelp_corp = tm_map(yelp_corp, content_transformer(removeNumbers))

yelp_dtm = DocumentTermMatrix(yelp_corp, 
                              control=list(weighting=weightTfIdf))
yelp_dtm = removeSparseTerms(yelp_dtm, .99)
yelp_text_df = as.matrix(yelp_dtm)
yelp_text_df = as.data.frame(yelp_text_df)


# Do some PCA here - clustering definitely isnt going to work on this much data
yelp_pc = prcomp(yelp_text_df, scale=FALSE)
loadings = yelp_pc$rotation
scores = yelp_pc$x

wss = (nrow(scores)-1)*sum(apply(scores, 2, var))
for (i in 2:15){
    print(i)
    wss[i] = sum(kmeans(scores[,c(1:10)],centers=i)$withinss)
} 

plot(1:15, wss, type="b", xlab="Number of Clusters",
     ylab="Within groups sum of squares")

cluster_all = kmeans(scores[,c(1:10)], centers=2, nstart=50)
scatterplot3d(scores[,1], y=scores[,2], z=scores[,3], color=cluster_all$cluster, angle=40)

a = which(cluster_all$cluster == 1)
b = which(cluster_all$cluster == 2)
summary(yelp_targets[b,])
summary(yelp_targets)
