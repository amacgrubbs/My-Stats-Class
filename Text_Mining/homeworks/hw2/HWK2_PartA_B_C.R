library(RTextTools)
library(caTools)
library(nnet)
library(tree)
library(caret)

yelp_data = read.csv("Yelp Data Restaurant Reviews Ratings.csv")
yelp_data = yelp_data[-7255,]
yelp_data$rating = ifelse(yelp_data$stars > 3,1,0)

#yelp_500 = yelp_data[1:500,]

#num_container = create_container(yelp_data[,2:20], yelp_data$rating, trainSize = 1:15000, testSize = 15001:19998, virgin = FALSE)

#Function to balance dataset:
#for col range use the ranges (startCol:endCol)
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

num_container = balanced_data(yelp_data,2:20,0.75,22)

num_RF = train_model(num_container, "RF")
num_BOOSTING = train_model(num_container, "BOOSTING")
num_SVM = train_model(num_container, "SVM")

num_RF_CLASSIFY = classify_model(num_container, num_RF)
num_BOOSTING_CLASSIFY = classify_model(num_container, num_BOOSTING)
num_SVM_CLASSIFY = classify_model(num_container, num_SVM)

num_analytics = create_analytics(num_container, cbind(num_RF_CLASSIFY, num_BOOSTING_CLASSIFY, num_SVM_CLASSIFY))

num_FOREST_CM = confusionMatrix(num_analytics@document_summary$MANUAL_CODE,num_analytics@document_summary$FORESTS_LABEL)
#num_FOREST_CM$overall[1]

num_BOOST_CM = confusionMatrix(num_analytics@document_summary$MANUAL_CODE,num_analytics@document_summary$LOGITBOOST_LABEL)
#num_BOOST_CM$overall[1]

num_SVM_CM = confusionMatrix(num_analytics@document_summary$MANUAL_CODE,num_analytics@document_summary$SVM_LABEL)
#num_SVM_CM$overall[1]


num_RF_CROSS = cross_validate(num_container, 5, "RF")
num_BOOSTING_CROSS = cross_validate(num_container, 5, "BOOSTING")
num_SVM_CROSS = cross_validate(num_container, 5, "SVM")

#num_RF_CROSS$meanAccuracy

#num_BOOSTING_CROSS$meanAccuracy

#num_SVM_CROSS$meanAccuracy


yelp_matrix = create_matrix(yelp_data$Review, language = "english", removeNumbers = TRUE, stemWords = TRUE, removeSparseTerms = .90)
#text_container = create_container(yelp_matrix, yelp_data$rating, trainSize = 1:15000, testSize = 15001:19998, virgin = FALSE)

text_df = as.data.frame(as.matrix(yelp_matrix))

mixed=cbind(text_df,yelp_data[,2:22])

text_container = balanced_data(mixed,1:92,0.75,113)

text_RF = train_model(text_container, "RF")
text_BOOSTING = train_model(text_container, "BOOSTING")
text_SVM = train_model(text_container, "SVM")

text_RF_CLASSIFY = classify_model(text_container, text_RF)
text_BOOSTING_CLASSIFY = classify_model(text_container, text_BOOSTING)
text_SVM_CLASSIFY = classify_model(text_container, text_SVM)

text_analytics = create_analytics(text_container, cbind(text_RF_CLASSIFY, text_BOOSTING_CLASSIFY, text_SVM_CLASSIFY))

text_FOREST_CM = confusionMatrix(text_analytics@document_summary$MANUAL_CODE,text_analytics@document_summary$FORESTS_LABEL)
#text_FOREST_CM$overall[1]

text_BOOST_CM = confusionMatrix(text_analytics@document_summary$MANUAL_CODE,text_analytics@document_summary$LOGITBOOST_LABEL)
#text_BOOST_CM$overall[1]

text_SVM_CM = confusionMatrix(text_analytics@document_summary$MANUAL_CODE,text_analytics@document_summary$SVM_LABEL)
#text_SVM_CM$overall[1]

text_RF_CROSS = cross_validate(text_container, 5, "RF")
text_BOOSTING_CROSS = cross_validate(text_container, 5, "BOOSTING")
text_SVM_CROSS = cross_validate(text_container, 5, "SVM")

#text_RF_CROSS$meanAccuracy

#text_BOOSTING_CROSS$meanAccuracy

#text_SVM_CROSS$meanAccuracy


#mixed_container = create_container(mixed, yelp_data$rating, trainSize = 1:15000, testSize = 15001:19998, virgin = FALSE)

mixed_container = balanced_data(mixed,1:111,0.75,113)


mixed_RF = train_model(mixed_container, "RF")
mixed_BOOSTING = train_model(mixed_container, "BOOSTING")
mixed_SVM = train_model(mixed_container, "SVM")

mixed_RF_CLASSIFY = classify_model(mixed_container, mixed_RF)
mixed_BOOSTING_CLASSIFY = classify_model(mixed_container, mixed_BOOSTING)
mixed_SVM_CLASSIFY = classify_model(mixed_container, mixed_SVM)

mixed_analytics = create_analytics(mixed_container, cbind(mixed_RF_CLASSIFY, mixed_BOOSTING_CLASSIFY, mixed_SVM_CLASSIFY))

mixed_FOREST_CM = confusionMatrix(mixed_analytics@document_summary$MANUAL_CODE,mixed_analytics@document_summary$FORESTS_LABEL)
#mixed_FOREST_CM$overall[1]

mixed_BOOST_CM = confusionMatrix(mixed_analytics@document_summary$MANUAL_CODE,mixed_analytics@document_summary$LOGITBOOST_LABEL)
#mixed_BOOST_CM$overall[1]

mixed_SVM_CM = confusionMatrix(mixed_analytics@document_summary$MANUAL_CODE,mixed_analytics@document_summary$SVM_LABEL)
#mixed_SVM_CM$overall[1]

mixed_RF_CROSS = cross_validate(mixed_container, 5, "RF")
mixed_BOOSTING_CROSS = cross_validate(mixed_container, 5, "BOOSTING")
mixed_SVM_CROSS = cross_validate(mixed_container, 5, "SVM")

#mixed_RF_CROSS$meanAccuracy

#mixed_BOOSTING_CROSS$meanAccuracy

#mixed_SVM_CROSS$meanAccuracy

accuracy_data <- data.frame(Num=c(num_FOREST_CM$overall[1],num_BOOST_CM$overall[1],num_SVM_CM$overall[1]),Num_X=c(num_RF_CROSS$meanAccuracy,num_BOOSTING_CROSS$meanAccuracy,num_SVM_CROSS$meanAccuracy),Text=c(text_FOREST_CM$overall[1],text_BOOST_CM$overall[1],text_SVM_CM$overall[1]),Text_X=c(text_RF_CROSS$meanAccuracy,text_BOOSTING_CROSS$meanAccuracy,text_SVM_CROSS$meanAccuracy),Mixed=c(mixed_FOREST_CM$overall[1],mixed_BOOST_CM$overall[1],mixed_SVM_CM$overall[1]),Mixed_X=c(mixed_RF_CROSS$meanAccuracy,mixed_BOOSTING_CROSS$meanAccuracy,mixed_SVM_CROSS$meanAccuracy))

barplot(as.matrix(accuracy_data),beside=TRUE,ylab="Accuracy",ylim=c(0.3,1),xpd=FALSE)
