library(RTextTools)
library(caTools)
library(nnet)
library(tree)
library(caret)

#Load the entireset 
yelp_data = read.csv("Yelp Data Restaurant Reviews Ratings.csv")
dim(yelp_data)
yelp_data$rating = ifelse(yelp_data$stars > 3,1,0)

#Function to balance dataset:
#for col range use the ranges (startCol:endCol)
balanced_data <- function(df,col_range, tSplit=0.75,Vsplit= 22){
        data_index = c(1:length(df))
        index <- c(1:length(df))
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
        ret_container = create_container(df[,col_range], yelp_data$rating, trainSize = train_index, testSize = test, virgin = FALSE)
        return(ret_container)
}


#Example Number conainer below when creating an X matrix of values from columns 2:20
num_container = balanced_data(yelp_data,2:20,0.75,22)
num_container
