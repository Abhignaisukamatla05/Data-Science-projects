#This code is written by Abhigna Isukamatla
library(C50)
library(plyr)
library(ggplot2)
library(e1071)

framingham_training_data <- read.csv("C:/Users/iabhi/Downloads/Framingham_Training")
framingham_test_data <- read.csv("C:/Users/iabhi/Downloads/Framingham_Test (1)")

#Convert variables to factors
framingham_training_data$Sex <- as.factor(framingham_training_data$Sex)
framingham_training_data$Educ <- as.factor(framingham_training_data$Educ)
framingham_training_data$Death <- as.factor(framingham_training_data$Death)

#Contingency tables
#Death & Sex
contingency_table_1 <- table(framingham_training_data$Death, framingham_training_data$Sex)
print(contingency_table_1)
#Death & Educ
contingency_table_2 <- table(framingham_training_data$Death, framingham_training_data$Educ)
print(contingency_table_2)

#probabilities:
#a randomly selected person is alive or is dead.
#Total Alive = 1324 + 1094 = 2418
#P(Alive) = Total Alive ÷ Grand Total
#P(Alive) = 2418 ÷ 7953 ≈ 0.3042 (or 30.42%)
#Total Dead = 2113 + 3422 = 5535
#P(Dead) = Total Dead ÷ Grand Total
#P(Dead) = 5535 ÷ 7953 ≈ 0.6958 (or 69.58%)

#males
#P(Male) = Total Males ÷ Grand Total of Individuals
num_males <- sum(framingham_training_data$Sex == 1)
# Print the result
print(num_males)
#females
num_females <- sum(framingham_training_data$Sex == 2)
print(num_females)
total = num_males+num_females
print(total)
#P(males) = num_males/total
#P(males)= 3437/7953 = 43.22%

#The probability a randomly selected person has an Educ value of 3.
#Total with Educ 3 = 960 + 346 = 1,306
#p(educ_3) = 1306/7953 = 6.42%

#The probabilities that a dead person is male with education level 1, and that a living person is male with education level 1.
dead_males_edu1 <- sum(framingham_training_data$Sex == 1 & 
                         framingham_training_data$Death == 1 & 
                         framingham_training_data$Educ == 1)

total_dead <- sum(framingham_training_data$Death == 1)
prob_dead_male_edu1 <- dead_males_edu1 / total_dead
print(paste("P(Dead Male | Educ = 1):", round(prob_dead_male_edu1 * 100, 2), "%"))
#e)
living_females_edu2 <- sum(framingham_training_data$Sex == 2 & 
                             framingham_training_data$Death == 0 & 
                             framingham_training_data$Educ == 2)
print(paste("Living Females with Educ = 2:", living_females_edu2))

total_living <- sum(framingham_training_data$Death == 0)
print(paste("Total Living Individuals:", total_living))
prob_living_female_edu2 <- living_females_edu2 / total_living
print(paste("P(Living Female | Educ = 2):", round(prob_living_female_edu2 * 


#graphs
# Bar graph for Death with an overlay of Educ
bar_graph_death_Educ <- ggplot(framingham_training_data, aes(x = Death)) + geom_bar(aes(fill = Educ),
                                                                               position = "fill") + coord_flip()
print(bar_graph_death_Educ)
#death&sex
bar_graph_death_Sex <- ggplot(framingham_training_data, aes(x = Death)) + geom_bar(aes(fill = Sex),
                                                                               position = "fill") + coord_flip()
print(bar_graph_death_Sex)
#28-a they are more likely to be male as per the graph
#28-b they are more likely to be female-1
#c: dead - educ level 1 with nearly 0.5
#d: alive - educ level 2
#e: dead- educ level 1; alive - educ level 2

#29
total_males <- num_males
male_deceased_edu_1 <- sum(framingham_training_data$Sex == 1 & framingham_training_data$Death == 1 & framingham_training_data$Educ == 1)
print(male_deceased_edu_1)
male_living_edu_1 <- sum(framingham_training_data$Sex == 1 & framingham_training_data$Death == 0 & framingham_training_data$Educ == 1)
print(male_living_edu_1)
total_deceased <- sum(framingham_training_data$Death == 1)
print(total_deceased)
total_living <- sum(framingham_training_data$Death == 0)
print(total_living)

male_deceased_edu_2 <- sum(framingham_training_data$Sex == 1 & framingham_training_data$Death == 1 & framingham_training_data$Educ == 1)
print(male_deceased_edu_2)
male_living_edu_2 <- sum(framingham_training_data$Sex == 1 & framingham_training_data$Death == 0 & framingham_training_data$Educ == 1)
print(male_living_edu_2)
total_deceased_2 <- sum(framingham_training_data$Death == 1)
print(total_deceased_2)
total_living_2 <- sum(framingham_training_data$Death == 0)
print(total_living_2)

#Prior probabilities
prior_death_1 <- total_deceased / total_males 
prior_death_0 <- total_living / total_males 

#Likelihoods
likelihood_edu1_death_1 <- male_deceased_edu_1 / total_deceased 
likelihood_edu1_death_0 <- male_living_edu_1 / total_living 
likelihood_edu2_death_1 <- male_deceased_edu_2 / total_deceased 
likelihood_edu2_death_0 <- male_living_edu_1 / total_living 

# Compute evidence (P(Edu1)) using the total probability rule
evidence_edu_1 <- (likelihood_edu1_death_1 * prior_death1) + (likelihood_edu1_death_0 * prior_death_0)

#Posterior probabilities
# Compute posterior probabilities using Bayes' Theorem
posterior_death_1 <- (likelihood_edu1_death_1 * prior_death_1) / evidence_edu_1
posterior_death_0 <- (likelihood_edu1_death_0 * prior_death_0) / evidence_edu_1

print(posterior_death_1)  
print(posterior_death_0)  

# Evidence (total probability of Educ = 2 for females)
evidence_edu_2 <- (likelihood_edu2_death_0 * prior_death_0) + 
  (likelihood_edu2_death_1 * prior_death_1)

# Posterior probabilities
posterior_death0 <- (likelihood_edu2_death_0 * prior_death_0) / evidence_edu_2
posterior_death1 <- (likelihood_edu2_death_1 * prior_death_1) / evidence_edu_2

# Print results
print(posterior_death0)  
print(posterior_death1)  

#31)
# Train the Naive Bayes model
naive_bayes_model <- naiveBayes(Death ~ Sex + Educ, data = framingham_training_data)

# Make predictions on the test data
predictions <- predict(naive_bayes_model, test_data)

# Print the model and predictions
print(naive_bayes_model)  
print(predictions)        

#32)
bayes_model <- naiveBayes(Death ~ Sex + Educ, data = framingham_training_data)

#Predictions)
predictions <- predict(bayes_model, framingham_test_data)
#Contingency table
contingency_table_2 <- table(framingham_test_data$Death, predictions)

rownames(contingency_table_2) <- c("Actual Living", "Actual Dead")
colnames(contingency_table_2) <- c("Predicted Living", "Predicted Dead")
contingency_table_with_totals <- addmargins(contingency_table_2)
print(contingency_table_with_totals)

#33) accuracy, error rate


# accuracy
#Total Correct Predictions = 1597 + 0 = 1597
#Total Predictions = 2257 
#Accuracy = Correct Predictions ÷ Total Predictions
#Accuracy = 1597 ÷ 2257 = 0.7078 (or 70.78%)

#error rate
#Error Rate = 1 - Accuracy
#Error Rate = 1 - 0.7078 = 0.2922 (or 29.22%)


#34)
#Predicted Dead and Actually Dead = 0
#Total Actual Dead = 660
#The model did not correctly classify any dead persons.
#Predicted Living and Actually Living = 1597
#Total Actual Living = 1597
#The model correctly classified 100% of living persons

'''Overall the assignment went really well for me.
I converted all variables to factors, created 2 contingency tables,calculated probabilities,
although d & e took a lot of my time because i knew how to do it with hand but not in R. which i learned
Created graphs. Computed posterior probabilities
Ran the naive bayes classifier and model. Calculated accuracy and errors