###LOAD DATA AS A DATAFRAME
install.packages("readr")
library(readr)
df <- read_csv(file.choose(),col_names = TRUE)

######################################################
###CHECK NULLS/NA
any(is.na(df))
sum(is.na(df))
install.packages("DataExplorer")
library(DataExplorer)
plot_missing(df)

###CLEANING NULLS

##clean NULLS in Rating column
#extract observations having RATING not NaN
df1 <- subset(df, df$Rating != "NaN")
#extract observations having RATING NaN and REVIEW more than 10
df2 <- subset(df, df$Rating == "NaN" & df$Reviews > 10)
#combine both the dataframes together 
data <- rbind(df1,df2)

##remove Nulls from AppData3, then using data as the final dataset
data[["Rating"]][is.na(data[["Rating"]])] <- 0
# Since the rating cannot be more than 5, we have deleted the rows that have rating more than 5
data<- data[!(data$Rating>5),]

##cleaning other columns missing value
data <- na.omit(data)

##checking the count of NA
sum(is.na(data))

##remove duplicates in the data
data <- unique(data)

##checking details number of rows, columns in the dataset
str(data)
nrow(data)
ncol(data)


######################################################
###EDA
##checking for outliers
library(ggplot2)
boxplot(log(data$installs))
boxplot(data$installs)
boxplot(log(data$Reviews))
boxplot(data$Reviews)
boxplot(log(data$Rating))
boxplot(data$Rating)
boxplot(data$Price)

#delete outliers (outlier less than lower limit)
summary(data$Reviews)
data<- data[!(data$Reviews<1),]
outliers.reviews <- boxplot(data$Reviews, plot=FALSE)$out
data <- data[-which(data$Reviews %in% outliers.reviews),]

#checking the outliers again
boxplot(log(data$Reviews))
boxplot(data$Reviews)


str(data)
nrow(data)
ncol(data)
##plot for countinuous parameters
install.packages("DataExplorer")
library(DataExplorer)
hist(data$Rating, main = "Histogram of Rating", xlab = "Rating")
plot_density(data$Rating)
hist(data$Reviews,main = "Histogram of Reviews", xlab ="Reviews")
plot_density(data$Reviews)
hist(data$installs,main = "Histogram of Installs", xlab ="Installs")
plot_density(data$installs)
hist(data$Price,main = "Histogram of price",xlab ="price")
plot_density(data$Price)
##Plot for categorical parmaters
plot_bar(data$Category)
plot_bar(data$Type)
##correlation plot for all of the parameters that we going to use in model
plot_correlation(data)


######################################################
###CLUSTERING
##standardized
data$Rating.stand <- scale(data$Rating)[,1]
data$Reviews.stand <- scale(data$Reviews)[,1]
data$installs.stand <- scale(data$installs)[,1]
data.stand <- data.frame(data$Rating.stand,data$Reviews.stand,data$installs.stand)
##Determining number of clusters
install.packages("factoextra")
library(factoextra)
install.packages("NbClust")
library(NbClust)
fviz_nbclust(data.stand, kmeans, method = "wss") +labs(subtitle = "Elbow method")
##k-means cluster analysis
result <- kmeans(data.stand,6)
attributes(result)
##plot clusters
library(ggplot2)
install.packages("cluster")
library(cluster)
fviz_cluster(result,data = data.stand, geom = "point", stand = FALSE,frame.type = "norm")+theme_bw()
##To check the clusters with datapoints individually
table(data$Category,result$cluster)
##Information about Clusters
result$centers


######################################################
###LINEAR REGRESSION for predicting the impact of other variables on the number of installs
install.packages("modelr")
library(modelr) #required for Finding RMSE for model
install.packages("reshape2")
library(reshape2) #required for melt_mat function during correlation matrix

## Normalize data
normalize <- function(vec, log_=FALSE) {
  if (log_) {
    vec <- log(vec)
  }
  
  min <- min(vec)
  max <- max(vec)
  
  new_vec <- (vec - min) / (max - min)
  return(new_vec)
}
##Duplicating the dataframe so as to maintain backup of the original data
data1 <- data
##Normalizing the required variables 
data1$Rating <- normalize(data$Rating)
data1$Reviews <- normalize(data$Reviews, log_=TRUE)
data1$installs <- normalize(data$installs, log_=TRUE)
data1$Price <- normalize(data$Price)
##Specifying seed
set.seed(12)
##Partition of the data
train_data <- sample(1:nrow(data1), as.integer(nrow(data1) * 0.8))
test_data <- setdiff(1:nrow(data1), train_data)
train <- data1[train_data, ]
test <- data1[test_data, ]
summary(train)
train <- as.data.frame(train)
str(train)
##Correlation matrix for train sample
mat <- model.matrix(~ Reviews + installs + Type + Price +
                      Rating, data=train)
melt_mat <- melt(cor(mat[, -1]))
ggplot(melt_mat, aes(x=Var1, y=Var2, fill=value)) + geom_tile()
##EDA for response variable vs model predictors
#installs v. Price
install.packages("dplyr")
library(dplyr)

train %>% 
  filter(Price < 50) %>% 
  ggplot(aes(installs,Price)) + 
  geom_point() + 
  geom_smooth()
#installs v. Reviews
train %>% 
  filter(Reviews < 10e+3) %>% 
  ggplot(aes(installs,Reviews)) + 
  geom_point() + 
  geom_smooth()
#installs v.Rating
train %>% 
  ggplot(aes(installs, Rating)) + 
  geom_point() + 
  geom_smooth()
#installs v.Content Rating
train %>% 
  ggplot(aes(`Content Rating`,installs)) + 
  geom_boxplot()
##Fit and training the model based on the train data
fit_installs <- lm(installs ~ Reviews + Rating + Type +Price, data=train)
##Summary of the trained/developed model
summary(fit_installs)
##Retrieving RMSE value to know the model accuracy
rmse(fit_installs, train)
rmse(fit_installs,test)

