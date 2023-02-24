#HW 03

#3... HW03_Prelude

house <- housing_train
house$over180 <- as.factor(ifelse(house$SalePrice > 180000, "Over $180K", "Under $180K"))

subhouse <- subset(house, select = c(2:6, 8:10, 12, 16, 17, 20:25, 30, 36, 38:30, 32,34:35, 
                                     37:43, 47:53, 55, 57, 59:63, 66:69, 71, 72, 78, 82))
for(i in c(2, 5:11, 14:19, 22, 24:30, 35:38, 48, 50, 53, 60)){
  subhouse[,i] <- as.factor(subhouse[,i])
}

# Now it's your turn!

#Partitioning the Data Numerically
set.seed(69)

InTrain <- createDataPartition(y = subhouse$over180, p= .75, list = FALSE)


Train_Data <- subhouse[InTrain,]

Test_Data <- subhouse[-InTrain,]


Train_Data$trainortest <- rep("train", nrow(Train_Data))
Test_Data$trainortest <- rep("test", nrow(Test_Data))
All_Data <- rbind(Train_Data, Test_Data)


boxplot(LotArea ~ as.factor(trainortest), data = All_Data)

kruskal.test(LotArea ~ as.factor(trainortest), data = All_Data)$p.value


#Partitioning for Categorical Data
#Use the 'All_Data' data set
table(All_Data$trainortest, All_Data$Street)

Partitioning_Table <- table(All_Data$trainortest, All_Data$Street)

prop.test(Partitioning_Table, correct = FALSE)$p.value  


#Remove Sale Price from data set because it makes our data overly simplified

names(Train_Data)
Train_Data <- Train_Data[,-61]
names(Train_Data)

names(Test_Data)
Test_Data <- Test_Data[,-61]
names(Test_Data)

#Cart Model

Train_Cart <- rpart(over180 ~., data = Train_Data, method = "class")

rpart.plot(Train_Cart,type = 4, extra = 102)

Pred_Train_Cart <- predict(object = Train_Cart, newdata = Train_Data, type = "class")

table(Train_Data$over180, Pred_Train_Cart)

375 + 629 #= 1004

dim(Train_Data)[1] # = 1095

1004/1095 # = 0.916895

#Testing for Accuracy of our model (The long way)

#Baseline Model = 1st Node = Under $180k
672/1095 # = 0.6136986

#Children modes (Bottom nodes) added up then / by sample size
(282+10+49+10+108+34+9+47+455)/1095 # = 0.916895

#See how much we increased the accuracy over the baseline model  
(0.916895-0.6136986)/0.6136986 # = 0.4940477

#increased the accuracy of our baseline model by almost 50%


#Testing the accuracy of the model on the training data set the short way
table(Train_Data$over180, Pred_Train_Cart)
#We look at 375 + 629
dim(Train_Data)[1]
#Then we divide by 1095
(375+629)/1095 # = 0.916895

# We get the same number and then subtract and then divide by the baseline # 
(0.916895-0.6136986)/0.6136986 # = 0.4940477

# And we get the same #

#Now we compare our Training cart model predictions with our testing data
Pred_Test_Cart <- predict(object = Train_Cart, newdata = Test_Data, type = "class")

table(Test_Data$over180,Pred_Test_Cart)

123 + 207 # = 330

dim(Test_Data)[1] # = 365

330/365 # = 0.9041096

#Training & Test data have similar Accuracy performances (0.916895 & 0.9041096) which means that the data is consistent

#Now we'll compare our baseline data for our testing data set. 
Test_Cart <- rpart(over180 ~., data = Test_Data, method = "class")

rpart.plot(Test_Cart,type = 4, extra = 102)

Pred_Test_Cart <- predict(object = Test_Cart, newdata = Test_Data, type = "class")

table(Test_Data$over180, Pred_Test_Cart)

129+ 214 #= 343

dim(Test_Data)[1] # = 365

343/365 # = 0.939726


#The accuracy of our Test model is .9339726, for our baseline 0.6136986 (1st node)

#K-fold cross validation

set.seed(134)

Train_Control <- trainControl(method = "cv", number = 10)

Train_Data_CCA <- na.omit(Train_Data)

model <- train(over180 ~., data = Train_Data_CCA, method = "rpart", trControl = Train_Control)

print(model)

#This helped us find our cp (complexity Parameter) #. This # gives us a good balance between accuracy & complexity.
#This means we're not plotting every single data point (Making a crazy complex model) but, we are taking the most useful data points to plot a model.

plot(model$finalModel)

text(model$finalModel)

#Use Package Rattle to plot the final model
fancyRpartPlot(model$finalModel, cex = 1.2, main = "Cart Model for houses that sold for <$180")


?fancyRpartPlot

#Now lets look at how it performs:

pred_Kfold_model <- rpart.predict(object = model, newdata = Train_Data)
table(Train_Data$over180,pred_Kfold_model)

# Check for accuracy:

383+526 # = 909
dim(Train_Data)[1] #1095

909/1095 # = 0.830137

# Now its performance for things its never seen (use testing data)

pred_kfold <- rpart.predict(object = model, newdata = Test_Data)
table(Test_Data$over180, pred_kfold)

134 + 168 # = 302

dim(Test_Data)[1] # = 365

302/365 # = 0.8273973




