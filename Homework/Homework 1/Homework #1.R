#Homework #1


bank_train[c(2,4)] #Question 1
Age_Mariage <- bank_train[c(2,4)] 


bank_train$age[1:100] #Question 2
agefirst100 <- bank_train$age[1:100]

agefirst100/2 #Question 3


 
Age_Mariage$age[(Age_Mariage$age > 80)] #Question 4

Age_Mariage[(Age_Mariage$age > 80) & (Age_Mariage$marital == "single"),] #Question 5

describe(Age_Mariage$age) #Question 6

hist(Age_Mariage$age) #Question 7

test_scores <- rnorm(30,85,7) #Question 8
hist(test_scores)
