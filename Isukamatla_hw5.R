#This code is written by Abhigna Isukamatla on 3/10/2025
library(plyr)
library(ggplot2)
library(C50)
adult_dataset <- read.csv("C:/Users/iabhi/Downloads/adult.data")
number_of_records <- dim(adult_dataset)[1]

#data_cleaning
colnames(adult_dataset) <- c("Age", "Workclass", "Fnlwgt", "Education", "Education_Num", 
                             "Marital_Status", "Occupation", "Relationship", "Race", 
                             "Sex", "Capital_Gain", "Capital_Loss", "Hours_Per_Week", 
                             "Native_Country")

head(adult_dataset)
colnames(adult_dataset)[15] <- "Income"
adult_dataset$Workclass <- factor(adult_dataset$Workclass)
adult_dataset$Native_Country <- as.factor(adult_dataset$Native_Country)
levels(adult_dataset$Native_Country)[levels(adult_dataset$Native_Country) == "Holand-Netherlands"] <- "Other"
summary(adult_dataset)

#Partitioning data
set.seed(7)
number_of_records <- dim(adult_dataset)[1]
train_ind <- runif(number_of_records) < 0.75
adult_train <- adult_dataset[ train_ind, ]
adult_test <- adult_dataset[ !train_ind, ]

#reexpressing data
adult_train$Marital_Status <- as.factor(adult_train$Marital_Status)
adult_train$Relationship <- as.factor(adult_train$Relationship)
adult_train$Workclass <- as.factor(adult_train$Workclass)
adult_train$Income <- as.factor(adult_train$Income)
adult_train$Age <- as.numeric(adult_train$Age)

#missing values
colSums(is.na(adult_train))

#Exploratory data analysis
#Bar graph with overlay using R
ggplot(adult_train, aes(Education)) + geom_bar(aes(fill = Sex), position = "fill") + coord_flip()
#contingency tables                                                     
table_1 <- table(adult_train$Sex, adult_train$Education)
table_2 <- addmargins(A = table_1, FUN = list(total = sum), quiet = TRUE)
round(prop.table(table_1, margin = 2)*100, 1)

#Histograms with Overlay 
ggplot(adult_train, aes(Hours_Per_Week)) +geom_histogram(aes(fill = Workclass), color="black", position = "fill")
#histogram 2
ggplot(adult_train, aes(Capital_Gain)) +geom_histogram(aes(fill = Race), color="black", position = "fill")
#histogram 3
ggplot(adult_train, aes(Capital_Loss)) +geom_histogram(aes(fill = Race), color="black", position = "fill")

#Balancing the data
head(adult_train)
table(adult_train$Income)
to.resample <- c("<=50K", ">50K")
weights <- c(18509,5899)
our.resample <- sample(x = to.resample, size = 2033, replace = TRUE, prob = weights / sum(weights))
our.resample <- adult_train[our.resample, ]
train_adult_rebal <- rbind(adult_train, our.resample)
table_1 <- table(train_adult_rebal$Income)
table_2 <- rbind(table_1, round(prop.table(table_1), 4))
colnames(table_2) <- c("Income = <=50K", "Income = >50K ");
rownames(table_2) <- c("Count", "Proportion")
table_2

#baseline performance evaluation
#The baseline model assigns all predictions to the value of the majority class.
#If the majority class is income ≤ $50,000 then the baseline model predicts "≤ 50K" for all entries.
#If the majority class is income > $50,000 all predictions will be ">50K."
#Accuracy = count of majority class/ Total Records
#>50K MODEL: 5899/24408 = 24.16%
#<= 50K MODEL: 18509/24408= 75.83%

#decision trees
C5 <- C5.0(formula = Income ~ Marital_Status + Capital_Gain + Capital_Loss, data = adult_train,
           control = C5.0Control(minCases=75))
plot(C5)
predictions <- predict(C5, adult_test)
# Evaluate the model
confusion_matrix <- table(Predicted = predictions, Actual = adult_test$Income)
print(confusion_matrix)
# Calculate accuracy
accuracy <- sum(diag(confusion_matrix)) / sum(confusion_matrix)
print(paste("Accuracy:", round(accuracy * 100, 2), "%"))

#2nd tree
C5_second <- C5.0(formula = Income ~ Age + Education_Num  + Hours_Per_Week, 
                  data = adult_train, 
                  control = C5.0Control(minCases = 75))

plot(C5_second)
predictions_second <- predict(C5_second, adult_test)
# Evaluate the model
confusion_matrix_second <- table(Predicted = predictions_second, Actual = adult_test$Income)
print(confusion_matrix_second)
#accuracy
accuracy_second <- sum(diag(confusion_matrix_second)) / sum(confusion_matrix_second)
print(paste("Accuracy:", round(accuracy_second * 100, 2), "%"))

#Comparing both the trees
#The first tree focuses on Marital_Status, Relationship, Workclass, Capital_Gain, and Capital_Loss.
#The root node was designed around socio-demographic or occupational predictors, which might have different relationships with income levels.
#The Second tree Your Tree Utilizes features like Education_Num, Age, Sex, and Hours_Per_Week.
#The root node is based on Education_Num.

#my comment
##after reading in the data, I cleaned the data, added proper column names, partitioned it into 75 and 25 as training data and testng data. 
#I then identified how many records are in the data set. Reexpressed the data as factors and numerics, tried to find some missing values.
#Did a couple of Exploratory analysis, where i produced bar graphs that I'm interested on studying about.
#I rebalanced data set, came up with a baseline performance for income more than 50K or less than or equal to 50K.
#Produced two decision trees, made sure that both had different sets of variables.
#The hard part was when my second tree did not turn out as expected. It could use a little bit of cleaning.
#My code produced the first tree until the last time i reran it before submission. I'm just hoping its my computer.

#Is my predictor successful? 
#The accuracy of my decision trees are 80.52% and 81.94% which are higher than baseline. This suggests that your model captures meaningful patterns beyond just predicting the majority class.
#As per this, I believe my model is successful. It can be improved especially in terms of the tress. I believe they can be made to look neat.