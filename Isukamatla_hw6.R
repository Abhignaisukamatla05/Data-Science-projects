#This code is written by Abhigna Isukamatla on 29th march 2025
library(C50)
library(plyr)
library(caret)
clothing_training_data <- read.csv("clothing_store_training.csv")
clothing_test_data <- read.csv("clothing_test_data.csv")

head(clothing_training_data)
#Factoring
# Convert the Response column to a factor for classification
clothing_training_data$Response <- as.factor(clothing_training_data$Response)
clothing_test_data$Response <- as.factor(clothing_test_data$Response)

clothing_training_data$Response <- as.factor(clothing_training_data$Response)
Model_1 <- C5.0(Response ~ Days.since.Purchase + Number.of.Purchase.Visits + Sales.per.Visit, data = clothing_training_data)
plot(Model_1)               

#here!
predicted_responses <- predict(Model_1, clothing_test_data)
print(predicted_responses)
#contingency table
contingency_table <- table(Predicted = predicted_responses, Actual = clothing_test_data$Response)
print(contingency_table)
#Model_evaluation_table
# Extract values from contingency table
TP <- contingency_table["1", "1"]  # True Positives
TN <- contingency_table["0", "0"]  # True Negatives
FP <- contingency_table["0", "1"]  # False Positives
FN <- contingency_table["1", "0"]  # False Negatives

# Print the values to ensure correctness
print(paste("True Positives (TP):", TP))
print(paste("True Negatives (TN):", TN))
print(paste("False Positives (FP):", FP))
print(paste("False Negatives (FN):", FN))

# Total cases
GT <- sum(contingency_table)   # Grand Total
TAP <- sum(contingency_table["1", ])  # Total Actual Positives
TAN <- sum(contingency_table["0", ])  # Total Actual Negatives
TPP <- sum(contingency_table[, "1"])  # Total Predicted Positives

#cost_matrix
cost_matrix <- matrix(c(0, 4, 1, 0), 
                      nrow = 2, 
                      byrow = TRUE)
rownames(cost_matrix) <- c("0", "1")
colnames(cost_matrix) <- c("0", "1")
print(cost_matrix)
# Metrics
accuracy <- (TN + TP) / GT
error_rate <- 1 - accuracy
sensitivity <- TP / TAP
specificity <- TN / TAN
precision <- TP / TPP
F1 <- (2 * precision * sensitivity) / (precision + sensitivity)
F2 <- (5 * precision * sensitivity) / ((4 * precision) + sensitivity)
F0.5 <- (1.25 * precision * sensitivity) / ((0.25 * precision) + sensitivity)
overall_cost <- sum(contingency_table * cost_matrix)
print(accuracy)
print(error_rate)
print(sensitivity)
print(specificity)
print(precision)
print(F1)
print(F2)
print(F0.5)
evaluation_table <- data.frame(
  Metric = c("Accuracy", "Error rate", "Sensitivity", "Specificity", "Precision", "F1", "F2", "F0.5"),
  Formula = c(
    "(TN + TP) / GT",
    "1 - Accuracy",
    "TP / TAP",
    "TN / TAN",
    "TP / TPP",
    "2 * Precision * Sensitivity / (Precision + Sensitivity)",
    "5 * Precision * Sensitivity / ((4 * Precision) + Sensitivity)",
    "1.25 * Precision * Sensitivity / ((0.25 * Precision) + Sensitivity)"
  ),
  Value = round(c(accuracy, error_rate, sensitivity, specificity, precision, F1, F2, F0.5), 4)
)

print(evaluation_table)

#interpretation
#Accuracy With a value of 84.10% means that the model successfully classified the majority of test instances correctly, balancing both positive and negative classifications.
#Error rate at 15.90% means that approximately one in six predictions made by the model are wrong.
#Sensitivity With a value of 55.23%, the model struggles to capture all actual positives—it correctly identifies just over half of the positive cases.
#Specificity at 86.79%, the model performs well in recognizing negative cases, correctly identifying most of them.
#Precision at 28.04%, the model is less reliable when predicting positives, meaning that nearly three out of four positive predictions are false positives.
#The F1 Score at 37.20%, the F1 score indicates that the model is struggling to maintain a balance between precision and recall, particularly due to the low precision.
#The F2 Score at 46.26%, the model is somewhat better at recall-focused tasks but still falls short in critical areas.
#The F0.5 Score at 31.10%, the model struggles to maintain reliable precision for positive predictions.



#Model_2
clothing_training_data$Response <- as.factor(clothing_training_data$Response
Model_2 <- C5.0(Response ~ Days.since.Purchase + Number.of.Purchase.Visits + Sales.per.Visit, 
               data = clothing_training_data, 
               costs = cost_matrix)
plot(Model_2)
# Print summary of the model
summary(Model_2)

# Predict responses using Model 2
predicted_responses_model_2 <- predict(Model_2, clothing_test_data)
contingency_table_model_2 <- table(Predicted = predicted_responses_model_2, Actual = clothing_test_data$Response)
print(contingency_table_model_2)

#contingency_table_2
contingency_table_model_2 <- table(Predicted = predicted_responses_model_2, 
                                  Actual = clothing_test_data$Response)
TP <- contingency_table_model_2["1", "1"]  # True Positives
TN <- contingency_table_model_2["0", "0"]  # True Negatives
FP <- contingency_table_model_2["0", "1"]  # False Positives
FN <- contingency_table_model_2["1", "0"]  # False Negatives

print(paste("True Positives (TP):", TP))
print(paste("True Negatives (TN):", TN))
print(paste("False Positives (FP):", FP))
print(paste("False Negatives (FN):", FN))

#Accuracy_2
accuracy_2 <- (TP + TN) / sum(contingency_table_model_2)
print(paste("Accuracy:", accuracy_2))
#Sensitivity, Precison_2
sensitivity_2 <- TP / (TP + FN)  
specificity_2 <- TN / (TN + FP)
precision_2 <- TP / (TP + FP)

print(paste("Sensitivity:", sensitivity_2))
print(paste("Specificity:", specificity_2))
#F1_2
F1_2 <- (2 * precision * sensitivity_2) / (precision_2 + sensitivity_2)
print(paste("F1-Score:", F1_2))
#F2_2
F2_2 <- (5 * precision_2 * sensitivity_2) / ((4 * precision_2) + sensitivity_2)
#F0.5._2
F0.5_2 <- (1.25 * precision_2 * sensitivity_2) / ((0.25 * precision_2) + sensitivity_2)

#overall_cost_2
overall_cost_2 <- sum(contingency_table_model_2 * cost_matrix)
print(paste("Overall Cost:", overall_cost_2))

#Profit_per_person
total_revenue <- 10000 
total_customers <- nrow(clothing_test_data)
profit_2 <- (total_revenue - overall_cost) / total_customers
print(paste("Profit per Customer:", profit_per_2))

#Comparsion_table
comparison_table <- data.frame(
  Metric = c("Accuracy", "Sensitivity", "Specificity", "Precision", 
             "F1 Score", "F2 Score", "F0.5 Score", "Overall Cost", "Profit per Customer"),
  Model_1 = c(accuracy, sensitivity, specificity, 
              precision, F1, F2, F0.5, NA, NA),
  Model_2 = c(accuracy_2, sensitivity_2, specificity_2, 
              precision_2, F1_2, F2_2, F0.5_2, overall_cost_2, profit_2)
)

# Print the comparison table
print(comparison_table)
#Comparsions:
#Precision((68.14%):Model 2 excels in precision, meaning most of its positive predictions are accurate. This is critical when avoiding false positives is more important, such as saving resources in targeted campaigns.
#Specificity (92.06%): It performs exceptionally well at identifying negative responses, avoiding false positives and ensuring accurate identification of non-responding customers.
#Cost Optimization: Model 2 achieves a significantly reduced overall cost (4923), making it ideal for cost-sensitive applications.
#Profitability: With a calculated profit per customer of 0.3440, Model 2 aligns better with financial goals, prioritizing profitable outcomes over general accuracy.
#Lower Accuracy (73.44%): Model 2 sacrifices overall accuracy, with more misclassifications in both positive and negative cases

#Block_Comment:
#Profit_per_person was hard to calculate since i wasn't sure about the total revenue.
#I had a bit of hard time comparing especially since I felt that I wasn't so sure what all the metrics actuaaly meant and the Sensitivity was 35.04% which i thought it meant many positive responses are missed (false negatives). 
# I also struggled with Imbalanced Metrics that is F1 having a score of 19.04% and F2 score of 38.81% for Model 2. I thought It heavily prioritizes precision at the expense of recall.
#I had trouble building the comparsion table too but i took the help of my cousin to do so.
#Overall I felt that this assignment has my best work.