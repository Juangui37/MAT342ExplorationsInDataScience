# Partition + Validating Extra Credit 
#(Do 5 from any numeric data (Use kruskal.test ) on any datasets)

#Load Batting Data Set from Data sets folder & Caret Package


#Partitioning + Validating on Batting Data Set

#(1) Batting$G

set.seed(1)

inTrain_battingG <- createDataPartition( y = Batting$G , p= .80, list = FALSE)

BattingG_train <- Batting[inTrain_battingG,]
BattingG_test <- Batting[-inTrain_battingG,]

BattingG_train$trainortest <- rep("train", nrow(BattingG_train))
BattingG_test$trainortest <- rep("test", nrow(BattingG_test))
BattingG_all <- rbind(BattingG_train, BattingG_test)

boxplot(G ~ as.factor(trainortest), data = BattingG_all)
#BoxPlots are similiar

kruskal.test(G ~ as.factor(trainortest), data = BattingG_all)$p.value

#(2) Batting$AG

set.seed(8)

inTrain_battingAB <- createDataPartition( y = Batting$AB , p= .80, list = FALSE)

BattingAB_train <- Batting[inTrain_battingAB,]
BattingAB_test <- Batting[-inTrain_battingAB,]

BattingAB_train$trainortest <- rep("train", nrow(BattingAB_train))
BattingAB_test$trainortest <- rep("test", nrow(BattingAB_test))
BattingAB_all <- rbind(BattingAB_train, BattingAB_test)

boxplot(AB ~ as.factor(trainortest), data = BattingAB_all)
#BoxPlots are similiar

kruskal.test(AB ~ as.factor(trainortest), data = BattingAB_all)$p.value

#(3) Batting$R
set.seed(8)

inTrain_battingR <- createDataPartition( y = Batting$R , p= .80, list = FALSE)

BattingR_train <- Batting[inTrain_battingR,]
BattingR_test <- Batting[-inTrain_battingR,]

BattingR_train$trainortest <- rep("train", nrow(BattingR_train))
BattingR_test$trainortest <- rep("test", nrow(BattingR_test))
BattingR_all <- rbind(BattingR_train, BattingR_test)

boxplot(R ~ as.factor(trainortest), data = BattingR_all)
#BoxPlots are similiar

kruskal.test(R ~ as.factor(trainortest), data = BattingR_all)$p.value

#(4) Batting$H
set.seed(8)

inTrain_battingH <- createDataPartition( y = Batting$H , p= .80, list = FALSE)

BattingH_train <- Batting[inTrain_battingH,]
BattingH_test <- Batting[-inTrain_battingH,]

BattingH_train$trainortest <- rep("train", nrow(BattingH_train))
BattingH_test$trainortest <- rep("test", nrow(BattingH_test))
BattingH_all <- rbind(BattingH_train, BattingH_test)

boxplot(H ~ as.factor(trainortest), data = BattingH_all)
#BoxPlots are similiar

kruskal.test(H ~ as.factor(trainortest), data = BattingH_all)$p.value

#(5) Batting$X2B
set.seed(99)

inTrain_battingX2B <- createDataPartition( y = Batting$X2B , p= .80, list = FALSE)

BattingX2B_train <- Batting[inTrain_battingX2B,]
BattingX2B_test <- Batting[-inTrain_battingX2B,]

BattingX2B_train$trainortest <- rep("train", nrow(BattingX2B_train))
BattingX2B_test$trainortest <- rep("test", nrow(BattingX2B_test))
BattingX2B_all <- rbind(BattingX2B_train, BattingX2B_test)

boxplot(X2B ~ as.factor(trainortest), data = BattingX2B_all)
#BoxPlots are similiar

kruskal.test(X2B ~ as.factor(trainortest), data = BattingX2B_all)$p.value

#Now 5 Categorical Validating & Partitioning
#Load adult01 data set

# (1) workclass
set.seed(66)
inTrain_adult <- createDataPartition(y = adult01$age, p = .75, list = FALSE)

inTrain_adultTrain <- adult01[inTrain_adult,]
inTrain_adultTest <- adult01[-inTrain_adult,]

inTrain_adultTrain$trainortest <- rep("train", nrow(inTrain_adultTrain))
inTrain_adultTest$trainortest <- rep("test", nrow(inTrain_adultTest))

inTrain_adult_All <- rbind(inTrain_adultTrain, inTrain_adultTest)

table(inTrain_adult_All$trainortest, inTrain_adult_All$workclass)

WorkClass_Partition_Table <- table(inTrain_adult_All$workclass, inTrain_adult_All$trainortest)

prop.test(WorkClass_Partition_Table, correct = FALSE)$p.value

# (2) Education
set.seed(22)
inTrain_adult <- createDataPartition(y = adult01$age, p = .75, list = FALSE)

inTrain_adultTrain <- adult01[inTrain_adult,]
inTrain_adultTest <- adult01[-inTrain_adult,]

inTrain_adultTrain$trainortest <- rep("train", nrow(inTrain_adultTrain))
inTrain_adultTest$trainortest <- rep("test", nrow(inTrain_adultTest))

inTrain_adult_All <- rbind(inTrain_adultTrain, inTrain_adultTest)

table(inTrain_adult_All$trainortest, inTrain_adult_All$education)

Education_Partition_Table <- table(inTrain_adult_All$education, inTrain_adult_All$trainortest)

prop.test(Education_Partition_Table, correct = FALSE)$p.value

#(3) Marital Status

set.seed(77)

table(inTrain_adult_All$marital.status, inTrain_adult_All$trainortest)

MaritalStatus_Partitioning_Table <- table(inTrain_adult_All$marital.status, inTrain_adult_All$trainortest)

prop.test(MaritalStatus_Partitioning_Table, correct = FALSE)$p.value

#(4) Income 

set.seed(55)
table(inTrain_adult_All$income,inTrain_adult_All$trainortest)
Income_Partitioning_Table <- table(inTrain_adult_All$income,inTrain_adult_All$trainortest)
prop.test(Income_Partitioning_Table, correct = FALSE)$p.value

