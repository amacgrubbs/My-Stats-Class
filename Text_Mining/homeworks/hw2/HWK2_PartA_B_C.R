library(RTextTools)
library(caTools)
library(nnet)
library(tree)
library(caret)



yelp_data = read.csv("Yelp Data Restaurant Reviews Ratings.csv")
yelp_data$rating = ifelse(yelp_data$stars > 3,1,0)
yelp_500 = yelp_data[1:1000,]

yelp_matrix = create_matrix(yelp_500$Review, language = "english", removeNumbers = TRUE, stemWords = TRUE, removeSparseTerms = .90)
num_container = create_container(yelp_500[,2:20], yelp_500$rating, trainSize = 1:250, testSize = 251:500, virgin = FALSE)

RF = train_model(num_container, "RF")
BOOSTING = train_model(num_container, "BOOSTING")
BAGGING = train_model(num_container, "BAGGING")
SVM = train_model(num_container, "SVM")
SLDA = train_model(num_container, "SLDA")

RF_CLASSIFY = classify_model(num_container, RF)
BOOSTING_CLASSIFY = classify_model(num_container, BOOSTING)
BAGGING_CLASSIFY = classify_model(num_container, BAGGING)
SVM_CLASSIFY = classify_model(num_container, SVM)
SLDA_CLASSIFY = classify_model(num_container, SLDA)

analytics = create_analytics(num_container, cbind(RF_CLASSIFY, BOOSTING_CLASSIFY, BAGGING_CLASSIFY, SVM_CLASSIFY, SLDA_CLASSIFY))
summary(analytics)

topic_summary = analytics@label_summary
alg_summary = analytics@algorithm_summary
ens_summary = analytics@ensemble_summary
doc_summary = analytics@document_summary

RF = cross_validate(num_container, 5, "RF")
BOOSTING = cross_validate(num_container, 5, "BOOSTING")
BAGGING = cross_validate(num_container, 5, "BAGGING")
SVM = cross_validate(num_container, 5, "SVM")
SLDA = cross_validate(num_container, 5, "SLDA")



RF = train_model(container, "RF")
BOOSTING = train_model(container, "BOOSTING")
BAGGING = train_model(container, "BAGGING")
SVM = train_model(container, "SVM")
SLDA = train_model(container, "SLDA")

RF_CLASSIFY = classify_model(container, RF)
BOOSTING_CLASSIFY = classify_model(container, BOOSTING)
BAGGING_CLASSIFY = classify_model(container, BAGGING)
SVM_CLASSIFY = classify_model(container, SVM)
SLDA_CLASSIFY = classify_model(container, SLDA)

analytics = create_analytics(container, cbind(RF_CLASSIFY, BOOSTING_CLASSIFY, BAGGING_CLASSIFY, SVM_CLASSIFY, SLDA_CLASSIFY))
summary(analytics)

topic_summary = analytics@label_summary
alg_summary = analytics@algorithm_summary
ens_summary = analytics@ensemble_summary
doc_summary = analytics@document_summary

RF = cross_validate(container, 5, "RF")
BOOSTING = cross_validate(container, 5, "BOOSTING")
BAGGING = cross_validate(container, 5, "BAGGING")
SVM = cross_validate(container, 5, "SVM")
SLDA = cross_validate(container, 5, "SLDA")

new = as.data.frame(as.matrix(yelp_matrix))

both=cbind(new,yelp_500[,2:20])

both_container = create_container(both, yelp_500$rating, trainSize = 1:250, testSize = 251:500, virgin = FALSE)

RF = train_model(both_container, "RF")
BOOSTING = train_model(both_container, "BOOSTING")
BAGGING = train_model(both_container, "BAGGING")
SVM = train_model(both_container, "SVM")
SLDA = train_model(both_container, "SLDA")

RF_CLASSIFY = classify_model(both_container, RF)
BOOSTING_CLASSIFY = classify_model(both_container, BOOSTING)
BAGGING_CLASSIFY = classify_model(both_container, BAGGING)
SVM_CLASSIFY = classify_model(both_container, SVM)
SLDA_CLASSIFY = classify_model(both_container, SLDA)

analytics = create_analytics(both_container, cbind(RF_CLASSIFY, BOOSTING_CLASSIFY, BAGGING_CLASSIFY, SVM_CLASSIFY, SLDA_CLASSIFY))
summary(analytics)

topic_summary = analytics@label_summary
alg_summary = analytics@algorithm_summary
ens_summary = analytics@ensemble_summary
doc_summary = analytics@document_summary

RF = cross_validate(both_container, 5, "RF")
BOOSTING = cross_validate(both_container, 5, "BOOSTING")
BAGGING = cross_validate(both_container, 5, "BAGGING")
SVM = cross_validate(both_container, 5, "SVM")
SLDA = cross_validate(both_container, 5, "SLDA")