#Creating a Partition

set.seed(87)

inTrain_bank <- createDataPartition( y = bank$response, p= .80, list = FALSE)

bank_train <- bank[inTrain_bank,]
bank_test <- bank[-inTrain_bank,]
#This is Partitioning the data

#For Kruskal-Waliis (Numerical DATA) We take this extra step:

bank_train$trainortest <- rep("train", nrow(bank_train))
bank_test$trainortest <- rep("test", nrow(bank_test))
bank_all <- rbind(bank_train, bank_test)

boxplot(age ~ as.factor(trainortest), data = bank_all)
#boxplots should be similiar


#Validate it

kruskal.test(age ~ as.factor(trainortest), data = bank_all)$p.value


#If p values are below .05 or .1... or the boxplots arent similiar/median isnt the same... 
#change the seed value & do it again


