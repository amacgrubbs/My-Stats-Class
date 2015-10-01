rm(list=ls())

#some ideas of sentiment analysis
#http://begriffs.com/posts/2015-02-25-text-mining-in-r.html
#Great R packages: tm, topicmodels, LDAvis, syzuhet
library(NLP)
library(tm)
library(gsl)
library(topicmodels)
library(SDMTools)
library(RTextTools)


yelp_senti=read.csv("yelp_SentiResults.csv")
attach(yelp_senti)
names(yelp_senti)

#still needs a balanced train and test set, with cross validation check
#working on that this weekend


hist(stars)
review_class = ifelse(stars<4, 0, 1)
sentiscore = SentStrength_pos+SentStrength_neg
sentiscore_abs = SentStrength_pos + abs(SentStrength_neg)
hist(sentiscore)
cor(stars,sentiscore) #correlation with star ratings is 0.44
cor(review_class,sentiscore) #worse with review_class, 0.40

yelp_senti$class=review_class
yelp_senti$sentiscore = sentiscore
yelp_senti$sentiscore_abs = sentiscore_abs

#single score
senti_glm = glm(yelp_senti$class~yelp_senti$sentiscore)
summary(senti_glm)
class_hat=predict(senti_glm)
class_true=yelp_senti$class
confusion.matrix(class_true, class_hat, threshold = 0.5)

#dual scores
sentidual_glm = glm(yelp_senti$class~yelp_senti$SentStrength_pos+yelp_senti$SentStrength_neg)
summary(sentidual_glm)
class_hatdual=predict(sentidual_glm)
class_truedual=yelp_senti$class
confusion.matrix(class_truedual, class_hatdual, threshold = 0.5)
#why is this exactly the same?

#with absolute (ambivalence score)
sentiabs_glm = glm(yelp_senti$class~yelp_senti$sentiscore+yelp_senti$sentiscore_abs)
summary(sentiabs_glm)
class_hatabs=predict(sentiabs_glm)
class_trueabs=yelp_senti$class
confusion.matrix(class_trueabs, class_hatabs, threshold = 0.4)










#######
#everything below here used as possible reference





#########
balanced_data <- function(df,col_range, tSplit,Vsplit){
  index <- c(1:nrow(df))
  train = sample(index, length(index)*tSplit)
  test = index[-train]
  min = min(table(df[train,Vsplit]))
  max = max(table(df[train,Vsplit]))
  splitCol = df[train,Vsplit]
  smaller_col = as.integer(names(table(splitCol)))[which(table(splitCol)==min)]
  larger_col = as.integer(names(table(splitCol)))[which(table(splitCol)==max)]
  df2 = data.frame(df[train,Vsplit])
  names(df2) <- 'rating'
  train_index_1 = sample(which(df2==larger_col),min)
  train_index_0 = which(df2==smaller_col)
  train_index = c(train_index_1,train_index_0)
  ret_container = create_container(df[,col_range], df$rating, trainSize = train_index, testSize = test, virgin = FALSE)
  return(ret_container)
}

num_container = balanced_data(yelp_senti,1:26,0.75,24)

#########
#http://andybromberg.com/sentiment-analysis/

#BL: need to load the sentistrength word lists
#load up word polarity list and format it
afinn_list <- read.delim(file='AFINN/AFINN-111.txt', header=FALSE, stringsAsFactors=FALSE)
names(afinn_list) <- c('word', 'score')
afinn_list$word <- tolower(afinn_list$word)

#categorize words as very negative to very positive and add some movie-specific words
vNegTerms <- afinn_list$word[afinn_list$score==-5 | afinn_list$score==-4]
negTerms <- c(afinn_list$word[afinn_list$score==-3 | afinn_list$score==-2 | afinn_list$score==-1], "second-rate", "moronic", "third-rate", "flawed", "juvenile", "boring", distasteful", "ordinary", "disgusting", "senseless", "static", "brutal", "confused", "disappointing", "bloody", "silly", "tired", "predictable", "stupid", "uninteresting", "trite", uneven", "outdated", "dreadful", "bland")
posTerms <- c(afinn_list$word[afinn_list$score==3 | afinn_list$score==2 | afinn_list$score==1], "first-rate", "insightful", "clever", "charming", "comical", "charismatic", "enjoyable", "absorbing", "sensitive", "intriguing", "powerful", "pleasant", "surprising", "thought-provoking", "imaginative", "unpretentious")
vPosTerms <- c(afinn_list$word[afinn_list$score==5 | afinn_list$score==4], "uproarious", "riveting", "fascinating", "dazzling", "legendary")

#he created a lot of code to run a classifier
classifier <- naiveBayes(results[,2:5], results[,6])

#confusion matrix
confTable <- table(predict(classifier, results), results[,6], dnn=list('predicted','actual'))
confTable