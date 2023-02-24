#Task 1 Question 1

#A
summary(adult01)

#b
boxplot_Age <- boxplot(adult01$age,xlab = "Age" ,horizontal = TRUE,col = "Blue", main = "Age Boxplot")
boxplot_Age

Histogram.Age <- hist(adult01$age, xlab = "Age", col = "lightBlue", main = "Age Histogram")


#c
#Evauluating categorical data using summary

summary(adult01)

#Evaluating categorical data using barplots

barplot.education <- ggplot(adult01) + 
  geom_bar(mapping = aes(x= education))

barplot.workclass <- ggplot(adult01) + 
  geom_bar(mapping = aes(x= workclass))

barplot.maritalstatues <- ggplot(adult01) + 
  geom_bar(mapping = aes(x= marital.status))

barplot.income <- ggplot(adult01) + 
  geom_bar(mapping = aes(x= income))

#using the package gridextra to make a one single photo for my grids.

grid.arrange(barplot.education,barplot.income,barplot.maritalstatues,barplot.workclass)

summary(adult01$income)
hist(adult01$age)

#Task 1 Question 2:

#Finding Z scores

minAge <- min(adult01$age)
maxAge <- max(adult01$age)

Minmax.Age <- (adult01$age - minAge) / (maxAge - minAge)

mean.Age <- mean(adult01$age)
sd.Age <- sd(adult01$age)

(minAge.zscore <- (minAge - mean.Age) / (sd.Age)) 
(meanAge.zscore <- (mean.Age - mean.Age) / (sd.Age))
(maxAge.zscore<- (maxAge - mean.Age) / (sd.Age))

Age_z <- (adult01$age - mean.Age) / (sd.Age)

summary(Age_z)

#Faster way using caret

preProcess_Age_Z <- preProcess(adult01[1],cmethod = c("center","scale"))
preProcess_Age_Z

age_z <- predict(preProcess_Age_Z, adult01[1])
View(age_z)
summary(age_z)

#Finding outliers through z-scores

age_z[age_z >= 3]
age_z[age_z < -3]

#Finding outliers through statistics

summary(age)

1.5*IQR(age) +48 #48 = 3rd quartile to find the upper bound

age <- adult01$age
age[age >= 78]

#Task2

#Question 3 Create a contingency 

#using descr package to do it
Married.Statues <- adult01$marital.status
Income <- adult01$income

CrossTable(Income, Married.Statues,prop.chisq = FALSE,prop.t = FALSE)

#In class Method: a contingency table of income & getting married
Income_Marital_table <- table(Married.Statues, Income)
Income_Marital_table
Income_Marital_Table_Sum <- addmargins(A = Income_Marital_table, margin = c(1,2), FUN = sum, quiet = TRUE)
Income_Marital_Table_Sum
Income_Marital_table_Percentage <- round(prop.table(Income_Marital_table, margin = 1)*100,2)
Income_Marital_table_Percentage



#Now to get a contingency table of Marital statues & Income
Marital_Income_table <- table(Income, Married.Statues)
Marital_Income_table
Marital_Income_table_Sum <- addmargins(A = Marital_Income_table, margin = c(1,2), FUN = sum, quiet = TRUE)
Marital_Income_table_Sum
Marital_Income_table_Percentage <- round(prop.table(Marital_Income_table, margin = 1)*100,2)
Marital_Income_table_Percentage

#Question 4 Create a bar chat of marital status with income overlay. Show stacked and normalized forms

#First We'll Do a regular bar chart: Married statues with Income
ggplot(adult01, aes(Married.Statues)) + geom_bar(aes(fill = Income), color = "black",position = "dodge") + xlab("Married Status") + ylab("Married Couple Count") + ggtitle("Bar Chart of Married Statues W/ Income")
#Now We'll do stacked
ggplot(adult01, aes(Married.Statues)) + geom_bar(aes(fill = Income), color = "black",position = "stack") + xlab("Married Status") + ylab("Married Couple Count") + ggtitle("Stacked Bar Chart of Married Statues W/ Income Overlay")
#Now we'll do Normalized
ggplot(adult01, aes(Married.Statues)) + geom_bar(aes(fill = Income), color = "black", position = "fill") + xlab("Married Status") + ylab("Income Proportion") + ggtitle("Normalized bar chart of Married statues W/ Income Overlay")

Income_Marital_table_Percentage


#Question 7: Make a contingency table of Income and Workclass
WorkClass <- adult01$workclass

Income_Workclass_table <- table(Income, WorkClass)
Income_Workclass_table
Income_Workclass_Table_Sum <- addmargins(A = Income_Workclass_table, margin = c(1,2), FUN = sum, quiet = TRUE)
Income_Workclass_Table_Sum
Income_Workclass_table_Percentage <- round(prop.table(Income_Workclass_table, margin = 2)*100,2)
Income_Workclass_table_Percentage

#Question 8: Create a bar chart of Workclass with Income overlay, in both stacked and normalized forms
#First We'll do a regular bar chart: workclass with Income
ggplot(adult01, aes(WorkClass)) + geom_bar(aes(fill = Income), color = "black",position = "dodge") + xlab("Work Class") + ylab("Workers Count") + ggtitle("Bar Chart of Workclass & Income")
#Now We'll do a stacked barchart
ggplot(adult01, aes(WorkClass)) + geom_bar(aes(fill = Income), color = "black",position = "stack") + xlab("Work Class") + ylab("Workers Count") + ggtitle("Stacked Bar Chart of Workclass & Income Overlay")
#Now we'll do Normalized
ggplot(adult01, aes(WorkClass)) + geom_bar(aes(fill = Income), color = "black", position = "fill") + xlab("Work Class") + ylab("Income Proportion") + ggtitle("Normalized bar chart of Work Class W/ Income Proportion Overlay") 

summary(WorkClass)

#Question 12:
#Stacked Histogram
ggplot(adult01, aes(age)) + geom_histogram(aes(fill = Income), color = "black", binwidth = 1, position = "stack") + xlab("Ages") + ylab("Income Proportion") + ggtitle("Stacked Histogram of Age with Income Overlay Villegas")
#Normalized Histogram
ggplot(adult01, aes(age)) + geom_histogram(aes(fill = Income), color = "black", binwidth = 1, position = "fill") + xlab("Ages") + ylab("Income Proportion") + ggtitle("Normalized Histogram of Age with Income Overlay Villegas")

summary(age)

Income_Age_table <- table(age,Income)
Income_Age_table
Income_Age_Table_Sum <- addmargins(A = Income_Age_table, margin = c(1,2), FUN = sum, quiet = TRUE)
Income_Age_Table_Sum
Income_AGE_table_Percentage <- round(prop.table(Income_Age_table, margin = 1)*100,2)
Income_AGE_table_Percentage
#TASK 4.... WRAPPING IT UP.

