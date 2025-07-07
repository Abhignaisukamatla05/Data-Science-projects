#This code is written by Abhigna Isukamatla on 02/18/2025
library(ggplot2)
library(readr)
bank_train = read_csv("bank_data.csv")

#Histogram of marital
ggplot(bank_train, aes(marital)) + geom_bar() + coord_flip()
#Histogram of marital with Overlay of response
ggplot(bank_train, aes(marital)) + geom_bar(aes(fill = response)) + coord_flip()
#Normalized Histogram
ggplot(bank_train, aes(marital)) + geom_bar(aes(fill = response),position = "fill") + coord_flip()
#Relationship:married people said yes, followed by single people, divorced and unknown
#But married people also said No the most

#Histogram of duration
ggplot(bank_train, aes(duration)) + geom_histogram(color="blue")
#Histogram of duration with an Overlay of response
ggplot(bank_train, aes(duration)) + geom_histogram(aes(fill = response), color="blue")
#normalized Histogram
ggplot(bank_train, aes(duration)) +geom_histogram(aes(fill = response), color="black", position = "fill")
#Relationship: as the duration of the call was increasing, people were saying yes more.  

#combining job categories
job_frequencies <- table(bank_train$job)
response_frequencies <- table(bank_train$job[bank_train$response == "yes"])
response_percentages <- response_frequencies / job_frequencies * 100
bank_train$job2 <- ifelse(response_percentages[bank_train$job] < 10, "0 < 10",
                  ifelse(response_percentages[bank_train$job] >= 10 & response_percentages[bank_train$job] < 25, "10 < 25",
                  ifelse(response_percentages[bank_train$job] >= 25 & response_percentages[bank_train$job] < 33, "25 < 33",
                                       "Other")))
job_categories_table <- table(bank_train$job, bank_train$job2)
print(job_categories_table)

#contingency table of job 2 with response. with counts and column %
table_1 <- table(bank_train$job2, bank_train$response)
table_2 <- addmargins(A = table_1, FUN = list(total = sum), quiet = TRUE)
round(prop.table(table_1, margin = 2)*100, 1)

#histogram of job2 and response
ggplot(bank_train, aes(job2)) + geom_bar() + coord_flip()
#normalised histogram of job2 with response
ggplot(bank_train, aes(job2)) + geom_bar(aes(fill = response),position = "fill") + coord_flip()

#relationship: retired and students who are in 25<33 category said yes more.
#Followed by people in admin,management,self-employed who are in 10<25 category.
#people who are blue-collar, entrepreneur, housemaid who are in 0<10 category said yes the least

#comment: Bargraphs with an overlay is better than normal bar graph to understand. 
#Normalised bar graph beats the both of them. Imight use normalised bar graph with an overlay from now on because of its clarity
#It took me a while to figure out the code for combining job categories according to the response percentages.
#I liked contingency table. It adds more clarity to the analysis.
#Overall this homework did not freak me out