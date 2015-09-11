yelp_data = read.csv("C:/Users/Zach/Google Drive/University of Texas/MSBA Fall Classes/Text Analysis/Homework_2/Yelp Data Restaurant Reviews Ratings.csv")
yelp_500 = yelp_data[1:500,]
names(yelp_500)
library(RTextTools)
library(ipred)
library(caTools)
library(nnet)
library(tree)
yep_matrix = create_matrix(yelp_500$Review, language = "english", removeNumbers = TRUE, stemWords = TRUE, removeSparseTerms = .90)
container = create_container(yep_matrix, yelp_500$stars, trainSize = 1:250, testSize = 251:500, virgin = FALSE)

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

write.csv(analytics@document_summary, "DocumentSummary.csv")
