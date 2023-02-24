summary(Vehicle)
#Make Target Variable
Vehicle$Budget <- as.factor(ifelse(Vehicle$USDPrice < 15001, "Under 15k", "Over 15k"))
summary(Vehicle)

#USE GGPlot Package

#Univariate Graphs 
library(ggplot2)
#Numeric Variables + Converting to Factors
boxplot_Year <- boxplot(Vehicle$Year,xlab = "Year" ,horizontal = TRUE,col = "Blue", main = "Year Boxplott")
Histogram_Year <- hist(Vehicle$Year, xlab = "Year", col = "lightBlue", main = "Year")

Vehicle$Year_C <- cut(Vehicle$Year,
                     breaks=c(1990, 2000, 2004, 2008, 2012 ,2016,2021),
                     labels=c( "1996 - 2000","2000 - 2004","2004 - 2008","2008 - 2012","2012 - 2016","2016 - 2020"))
(barplot_Year_C <- ggplot(Vehicle) + geom_bar(mapping = aes(x = Year_C)))

boxplot_MPG <- boxplot(Vehicle$MPG,xlab = "MPG" ,horizontal = TRUE,col = "yellow", main = "MPG Boxplott")
Histogram_MPG <- hist(Vehicle$MPG, xlab = "MPG", col = "lightBlue", main = "MPG Histogram")
Vehicle$MPG_C <- cut(Vehicle$MPG,
                            breaks=c(.1, 60, 120, 180, 240 ,300,360,420,490),
                            labels=c( "1 - 60k","60k - 120k","120k - 180k","180k - 240k","240k - 300k","300 - 360k","360k- 420k","420k - 480k"))
(barplot_MPG_c <- ggplot(Vehicle) + geom_bar(mapping = aes(x = MPG_C)))


boxplot_EngineSize <- boxplot(Vehicle$EngineSize,xlab = "Engine Size" ,horizontal = TRUE,col = "Blue", main = "Engine Size Boxplott")
Histogram_EngineSize <- hist(Vehicle$EngineSize, xlab = "Engine Size", col = "lightBlue", main = "Engine Size Histogram")
Vehicle$EngineSize_C <- cut(Vehicle$EngineSize,
                         breaks=c(-1, 1, 2, 3, 4, 5, 6 ,7),
                         labels=c( "0 - 1","1.1- 2","2 - 3","3 - 4","4 - 5","5 - 6","6- 7"))
(barplot_EngineSize_c <- ggplot(Vehicle) + geom_bar(mapping = aes(x = EngineSize_C)))


boxplot_Mileage <- boxplot(Vehicle$Mileage,xlab = "Mileage" ,horizontal = TRUE,col = "Blue", main = "Mileage Boxplott")
Histogram_Mileage <- hist(Vehicle$Mileage, xlab = "Mileage", col = "lightBlue", main = "Mileage Histogram")
Vehicle$Mileage_C <- cut(Vehicle$Mileage,
                         breaks=c(-1,40000, 80000, 120000, 160000, 200000, 260000),
                         labels=c("0-40k","41k-80k","81k-120k","121k-160k","161k-200k","201k-260k"))
(barplot_Mileage_c <- ggplot(Vehicle) + geom_bar(mapping = aes(x = Mileage_C)))

par(mfrow=c(2,2))
boxplot_Mileage <- boxplot(Vehicle$Mileage,xlab = "Mileage" ,horizontal = TRUE,col = "Blue", main = "Mileage Boxplott")
boxplot_EngineSize <- boxplot(Vehicle$EngineSize,xlab = "Engine Size" ,horizontal = TRUE,col = "Blue", main = "Engine Size Boxplott")
boxplot_MPG <- boxplot(Vehicle$MPG,xlab = "MPG" ,horizontal = TRUE,col = "yellow", main = "MPG Boxplott")
boxplot_Year <- boxplot(Vehicle$Year,xlab = "Year" ,horizontal = TRUE,col = "Blue", main = "Year Boxplott")
par(mfrow=c(2,2))
Histogram_Mileage <- hist(Vehicle$Mileage, xlab = "Mileage", col = "lightBlue", main = "Mileage Histogram")
Histogram_EngineSize <- hist(Vehicle$EngineSize, xlab = "Engine Size", col = "lightBlue", main = "Engine Size Histogram")
Histogram_MPG <- hist(Vehicle$MPG, xlab = "MPG", col = "lightBlue", main = "MPG Histogram")
Histogram_Year <- hist(Vehicle$Year, xlab = "Year", col = "lightBlue", main = "Year")
library(gridExtra)
grid.arrange(barplot_EngineSize_c, barplot_Mileage_c, barplot_MPG_c, barplot_Year_C)
#Categorical Variables

(barplot_FuelType <- ggplot(Vehicle) + geom_bar(mapping = aes(x = FuelType)))

(barplot_Make <- ggplot(Vehicle) + geom_bar(mapping = aes(x = Make)))

(barplot_Transmission <- ggplot(Vehicle) + geom_bar(mapping = aes(x = Transmission)))

#GridExtra Package
#All Barplots in one picture
grid.arrange(barplot_FuelType,barplot_Make,barplot_Transmission)

#Bivariate Variables Dodge Position
(FT_BP_Dodge <- ggplot(Vehicle, aes(FuelType)) + geom_bar(aes(fill = Budget), color = "black",position = "dodge") + xlab("Fuel Type") + ylab("Fuel Type Count") + ggtitle("Bar Chart of Fuel Type W/ Budget"))
(Make_BP_Dodge <- ggplot(Vehicle, aes(Make)) + geom_bar(aes(fill = Budget), color = "black",position = "dodge") + xlab("Make Type") + ylab("Make Count") + ggtitle("Bar Chart of Make W/ Budget"))
(Trans_BP_Dodge <-ggplot(Vehicle, aes(Transmission)) + geom_bar(aes(fill = Budget), color = "black",position = "dodge") + xlab("Transmission") + ylab("Transmission Count") + ggtitle("Bar Chart of Transmission W/ Budget"))
(Year_BP_Dodge <-ggplot(Vehicle, aes(Year_C)) + geom_bar(aes(fill = Budget), color = "black",position = "dodge") + xlab("Year") + ylab("Year Count") + ggtitle("Bar Chart of Year W/ Budget"))
(MPG_BP_Dodge <-ggplot(Vehicle, aes(MPG_C)) + geom_bar(aes(fill = Budget), color = "black",position = "dodge") + xlab("MPG") + ylab("MPG Count") + ggtitle("Bar Chart of MPG W/ Budget"))
(Engine_BP_Dodge <-ggplot(Vehicle, aes(EngineSize_C)) + geom_bar(aes(fill = Budget), color = "black",position = "dodge") + xlab("Engine Size") + ylab("Engine Size") + ggtitle("Bar Chart of Engine Size W/ Budget"))
(MA_BP_Dodge <-ggplot(Vehicle, aes(Mileage_C)) + geom_bar(aes(fill = Budget), color = "black",position = "dodge") + xlab("Mileage") + ylab("Mileage Count") + ggtitle("Bar Chart of Mileage W/ Budget"))
grid.arrange(FT_BP_Dodge,Make_BP_Dodge,Trans_BP_Dodge,Year_BP_Dodge,MPG_BP_Dodge,Engine_BP_Dodge,MA_BP_Dodge)

#Bivariate Variables stack Position
(FT_BP_Stack <- ggplot(Vehicle, aes(FuelType)) + geom_bar(aes(fill = Budget), color = "black",position = "stack") + xlab("Fuel Type") + ylab("Fuel Type Count") + ggtitle("Bar Chart of Fuel Type W/ Budget"))
(Make_BP_Stack <- ggplot(Vehicle, aes(Make)) + geom_bar(aes(fill = Budget), color = "black",position = "stack") + xlab("Make Type") + ylab("Make Count") + ggtitle("Bar Chart of Make W/ Budget"))
(Trans_BP_Stack <-ggplot(Vehicle, aes(Transmission)) + geom_bar(aes(fill = Budget), color = "black",position = "stack") + xlab("Transmission") + ylab("Transmission Count") + ggtitle("Bar Chart of Transmission W/ Budget"))
(Year_BP_Stack <-ggplot(Vehicle, aes(Year_C)) + geom_bar(aes(fill = Budget), color = "black",position = "stack") + xlab("Year") + ylab("Year Count") + ggtitle("Bar Chart of Year W/ Budget"))
(MPG_BP_Stack <-ggplot(Vehicle, aes(MPG_C)) + geom_bar(aes(fill = Budget), color = "black",position = "stack") + xlab("MPG") + ylab("MPG Count") + ggtitle("Bar Chart of MPG W/ Budget"))
(Engine_BP_Stack <-ggplot(Vehicle, aes(EngineSize_C)) + geom_bar(aes(fill = Budget), color = "black",position = "stack") + xlab("Engine Size") + ylab("Engine Size") + ggtitle("Bar Chart of Engine Size W/ Budget"))
(MA_BP_Stack <-ggplot(Vehicle, aes(Mileage_C)) + geom_bar(aes(fill = Budget), color = "black",position = "stack") + xlab("Mileage") + ylab("Mileage Count") + ggtitle("Bar Chart of Mileage W/ Budget"))
grid.arrange(FT_BP_Stack,Make_BP_Stack,Trans_BP_Stack,MPG_BP_Stack,Year_BP_Stack,MPG_BP_Stack,Engine_BP_Stack,MA_BP_Stack)

#Bivariate Variables fill position
(FT_BP_fill <- ggplot(Vehicle, aes(FuelType)) + geom_bar(aes(fill = Budget), color = "black",position = "fill") + xlab("Fuel Type") + ylab("Budget Proportion")) 
  #+ ggtitle("Bar Chart of Fuel Type Wtih Budget overlay"))
(Make_BP_fill <- ggplot(Vehicle, aes(Make)) + geom_bar(aes(fill = Budget), color = "black",position = "fill") + xlab("Make Type") + ylab("Budget Proportion"))
  #+ ggtitle("Bar Chart of Make With Budget overlay"))
#(Trans_BP_fill <-ggplot(Vehicle, aes(Transmission)) + geom_bar(aes(fill = Budget), color = "black",position = "fill") + xlab("Transmission") + ylab("Budget Proportion"))
  #+ ggtitle("Bar Chart of Transmission With Budget overlay"))
#(Year_BP_fill <-ggplot(Vehicle, aes(Year_C)) + geom_bar(aes(fill = Budget), color = "black",position = "fill") + xlab("Year") + ylab("Budget Proportion"))
#+ ggtitle("Bar Chart of Year With Budget overlay"))
(MPG_BP_fill <-ggplot(Vehicle, aes(MPG_C)) + geom_bar(aes(fill = Budget), color = "black",position = "fill") + xlab("MPG") + ylab("Budget Proportion"))
#+ ggtitle("Bar Chart of MPG With Budget overlay"))
(Engine_BP_fill <-ggplot(Vehicle, aes(EngineSize_C)) + geom_bar(aes(fill = Budget), color = "black",position = "fill") + xlab("Engine Size") + ylab("Budget Proportion"))
#+ ggtitle("Bar Chart of Engine Size With Budget overlay"))
(MA_BP_fill <-ggplot(Vehicle, aes(Mileage_C)) + geom_bar(aes(fill = Budget), color = "black",position = "fill") + xlab("Mileage") + ylab("Budget Proportion"))
#+ ggtitle("Bar Chart of Mileage With Budget overlay"))
grid.arrange(FT_BP_Stack,Make_BP_Stack,Trans_BP_fill,MPG_BP_fill,Year_BP_fill,MPG_BP_fill,Engine_BP_fill,MA_BP_fill)

#Trivariate Graphs 
summary(Vehicle)

#GGPMISC Package
#scatter plots with an overlay of the target variable 
ggplot(Vehicle, aes(x= Year, y = USDPrice, color = Budget)) + geom_point() +geom_smooth(se = FALSE)
(Scat_Y <- ggplot(Vehicle, aes(x= Year, y = USDPrice, color = Budget)) +geom_smooth()+ geom_point())


ggplot(Vehicle, aes(x= EngineSize, y = MPG, color = Budget)) + geom_point() +geom_smooth(se = FALSE)
(Scat_En <- ggplot(Vehicle, aes(x= EngineSize, y = MPG, color = Budget)) +geom_smooth()+ geom_point())

ggplot(Vehicle, aes(x= Mileage, y = USDPrice, color = Budget)) + geom_point() +geom_smooth(se = FALSE)
(Scat_Mil <- ggplot(Vehicle, aes(x= Mileage, y = USDPrice, color = Budget)) + geom_point() +geom_smooth())
grid.arrange(Scat_En,Scat_Mil,Scat_Y)

#Binning based on predictive value 
(Year_Bin_1 <- ggplot(Vehicle, aes(Year)) + geom_histogram(aes(fill = Budget), color = "black", binwidth = 1, position = "fill") + xlab("Year") + ylab("Budget Proportion") + ggtitle("Normalized Histogram of Car Year with Overlay of Budget"))
Vehicle$Year_Bin <- cut(Vehicle$Year, breaks = c(-.01,2016,2020),
                        labels = c("1996-2016","2016-2020"))
(Year_Bin_2 <- ggplot(Vehicle, aes(Year_Bin)) + geom_bar(aes(fill = Budget), position = "stack")+ ylab("Budget Proportion") + ggtitle("Filled Histogram of Car Year with Overlay of Budget"))
(Year_Bin_3 <- ggplot(Vehicle, aes(Year_Bin)) + geom_bar(aes(fill = Budget), position = "fill") + ylab("Budget Proportion") + ggtitle("Stacked Histogram of Car Year with Overlay of Budget"))
grid.arrange(Year_Bin_1,Year_Bin_2,Year_Bin_3)

(Engine_Bin_1 <-ggplot(Vehicle, aes(EngineSize)) + geom_histogram(aes(fill = Budget), color = "black", binwidth = .35, position = "fill") + xlab("Engine Size") + ylab("Budget Proportion") + ggtitle("Normalized Histogram of Car Engine size with Overlay of Budget"))
Vehicle$EngineSize_Bin <- cut(Vehicle$EngineSize, breaks = c(-.01,1.8,8),
                        labels = c("0 - 1.8","1.8 - 8"))
(Engine_Bin_2 <-ggplot(Vehicle, aes(EngineSize_Bin)) + geom_bar(aes(fill = Budget), position = "stack")+ ylab("Budget Proportion") + ggtitle("Stacked Histogram of Engine SZ with Overlay of Budget"))
  (Engine_Bin_3 <-ggplot(Vehicle, aes(EngineSize_Bin)) + geom_bar(aes(fill = Budget), position = "fill") + ylab("Budget Proportion") + ggtitle("Filled Histogram of Engine SZ with Overlay of Budget"))
grid.arrange(Engine_Bin_1,Engine_Bin_2,Engine_Bin_3)

(MPG_Bin_1 <-ggplot(Vehicle, aes(MPG)) + geom_histogram(aes(fill = Budget), color = "black", binwidth = 10, position = "fill") + xlab("MPG") + ylab("Budget Proportion") + ggtitle("Normalized Histogram of MPG with Overlay of Budget"))
Vehicle$MPG_Bin <- cut(Vehicle$MPG, breaks = c(-.01,100,480),
                              labels = c("1 - 100k","100k - 480k"))
(MPG_Bin_2 <-ggplot(Vehicle, aes(MPG_Bin)) + geom_bar(aes(fill =Budget), position = "stack")+ ylab("Budget Proportion") + ggtitle("Stacked Histogram of Car Year with Overlay of Budget"))
(MPG_Bin_3 <-ggplot(Vehicle, aes(MPG_Bin)) + geom_bar(aes(fill = Budget), position = "fill") + ylab("Budget Proportion") + ggtitle("Filled Histogram of Car Year with Overlay of Budget"))
grid.arrange(MPG_Bin_1,MPG_Bin_2,MPG_Bin_3)

#Contingency Table of Bins
Year_Bin_Table <- table(Vehicle$Budget, Vehicle$Year_Bin)
prop.table(Year_Bin_Table,2)

EngineSize_Bin_Table <- table(Vehicle$Budget, Vehicle$EngineSize_Bin)
prop.table(EngineSize_Bin_Table,2)

Mpg_Bin_Table <- table(Vehicle$Budget, Vehicle$MPG_Bin)
prop.table(Mpg_Bin_Table,2)

#Checking for Correlations
round(cor(Vehicle[, c(3,4,6,8,9,10)]),2)







#Partitioning
library(caret)
set.seed(99)

InTrain <- createDataPartition(y = Vehicle$Budget, p= .75, list = FALSE)


Train_Data <- Vehicle[InTrain,]

Test_Data <- Vehicle[-InTrain,]


Train_Data$trainortest <- rep("train", nrow(Train_Data))
Test_Data$trainortest <- rep("test", nrow(Test_Data))
All_Data <- rbind(Train_Data, Test_Data)
summary(Vehicle)
par(mfrow=c(2,3))
#Barplots for 1st condition
#boxplot(Vehicle$Year ~as.factor(trainortest), data = All_Data, horizontal = TRUE, main = "Vehicle Year Training & Test Boxplot", col = "blue")
#boxplot(Vehicle$Mileage ~ as.factor(trainortest), data = All_Data, horizontal = TRUE, main = "Vehicle Mileage Magnitude Training & Test Boxplot", col = "red")
#boxplot(Vehicle$EngineSize~ as.factor(trainortest), data = All_Data, horizontal = TRUE, main = "Vehicle Engine Size Training & Test Boxplot", col = "pink")
#boxplot(Vehicle$MPG~ as.factor(trainortest), data = All_Data, horizontal = TRUE, main = "Vehicle MPG Training & Test Boxplot", col = "purple" )
#boxplot(Vehicle$USDPrice~ as.factor(trainortest), data = All_Data, horizontal = TRUE, main = "Vehicle USDPrice & Test Boxplot", col = "darkblue")
#kruskal.test(Vehicle$Year ~ as.factor(trainortest), data = All_Data)
#kruskal.test(Vehicle$Mileage ~ as.factor(trainortest), data = All_Data)$p.value
#kruskal.test(Vehicle$EngineSize ~ as.factor(trainortest), data = All_Data)$p.value
#kruskal.test(Vehicle$MPG ~ as.factor(trainortest), data = All_Data)$p.value
#kruskal.test(Vehicle$USDPrice ~ as.factor(trainortest), data = All_Data)$p.value

#Partitioning for Categorical data
library(caret)
set.seed(99)

InTrain <- createDataPartition(y = Vehicle$Budget, p= .75, list = FALSE)


Train_Data <- Vehicle[InTrain,]

Test_Data <- Vehicle[-InTrain,]


Train_Data$trainortest <- rep("train", nrow(Train_Data))
Test_Data$trainortest <- rep("test", nrow(Test_Data))
All_Data <- rbind(Train_Data, Test_Data)

Trans_P_Table <- table(All_Data$Transmission, All_Data$trainortest)
Make_P_Table <- table(All_Data$Make, All_Data$trainortest)
Fuel_P_Table <- table(All_Data$FuelType, All_Data$trainortest)
Year_P_Table <- table(All_Data$Year_C, All_Data$trainortest)
MPG_P_Table <- table(All_Data$MPG_Bin, All_Data$trainortest)
E_P_Table <- table(All_Data$EngineSize_C, All_Data$trainortest)
Mi_P_Table <- table(All_Data$Mileage_C, All_Data$trainortest)


summary(Vehicle$Budget)
prop.test(Trans_P_Table, correct = FALSE)$p.value
prop.test(Make_P_Table, correct = FALSE)$p.value
prop.test(Fuel_P_Table, correct = FALSE)$p.value
prop.test(Year_P_Table, correct = FALSE)$p.value
prop.test(MPG_P_Table, correct = FALSE)$p.value


prop.test(E_P_Table, correct = FALSE)$p.value
prop.test(Mi_P_Table, correct = FALSE)$p.value
library(rpart)
library(rpart.plot)
#remove prices from training & testing data 
names(Train_Data)
Train_Data <- Train_Data[,-8]
names(Train_Data)
Train_Data <- Train_Data[,-9:-16]
names(Train_Data)
names(Test_Data)
Test_Data <- Test_Data[,-8]
names(Test_Data)
Test_Data <- Test_Data[,-9:-16]
names(Test_Data)

Train_Cart <- rpart(Budget ~., data = Train_Data, method = "class")
rpart.plot(Train_Cart,type = 2, extra = 104)

#Baseline of Model = 1st node 
#60%
#Testing the accuracy of the model on the training data
Pred_Train_Cart <- predict(object = Train_Cart, newdata = Train_Data, type = "class")
table(Train_Data$Budget, Pred_Train_Cart)
dim(Train_Data)[1]
(27218+17842)/51332 

(0.877815-.60)/0.60

#Testing Cart Model baseline & Accuracy
Test_Cart <- rpart(Budget ~., data = Test_Data, method = "class")
rpart.plot(Test_Cart,type = 2, extra = 104)
#Baseline Model (1st Node)
#60%

Pred_Test_Cart <- predict(object = Test_Cart, newdata = Test_Data, type = "class")
table(Test_Data$Budget, Pred_Test_Cart)

9267+5919 # = 15186

dim(Test_Data)[1] # = 17109

15186/17109 # =  0.887603

( 0.887603-.60)/.60

#K-Fold Cross Validation
set.seed(134)

Train_Control <- trainControl(method = "cv", number = 50)

Train_Data_CCA <- na.omit(Train_Data)
model <- train(Budget ~. , data = Train_Data_CCA, method = "rpart", trControl = Train_Control)
print(model)
#This helped us find our cp (complexity Parameter) #. This # gives us a good balance between accuracy & complexity.
#This means we're not plotting every single data point (Making a crazy complex model) but, we are taking the most useful data points to plot a model.

plot(model$finalModel)

text(model$finalModel)
library(rattle)
#Use Package Rattle to plot the final model
fancyRpartPlot(model$finalModel, cex = 1.2, main = "Cars over $15k CART Model")
fancyRpartPlot(model$finalModel, cex = 2)


?fancyRpartPlot

#Now lets look at how it performs:

pred_Kfold_model <- rpart.predict(object = model, newdata = Train_Data)
table(Train_Data$Budget,pred_Kfold_model)

# Check for accuracy:

25321+15950 # = 41271
dim(Train_Data)[1] #51332

41271/51332 # = 0.8040014

#how it outpreformed the baseline model
(0.8040014-.60)/.60

# Now its performance for things its never seen (use testing data)

pred_kfold <- rpart.predict(object = model, newdata = Test_Data)
table(Test_Data$Budget, pred_kfold)

8358+5259 # = 13167

dim(Test_Data)[1] # = 17109

13167/17109 # = 0.769595

5259/(1659+5259)

8358/(8358+1833)

#Final Pres
par(mfrow=c(2,2))
(ab <- ggplot(Vehicle, aes(Year)) + geom_bar(aes(fill = Budget), color = "black",position = "fill") + xlab("Year") + ylab("Budget Proportion"))
(cd <-ggplot(Vehicle, aes(Transmission)) + geom_bar(aes(fill = Budget), color = "black",position = "fill") + xlab("Transmission") + ylab("Budget Proportion"))


(FT_BP_fill <- ggplot(Vehicle, aes(FuelType)) + geom_bar(aes(fill = Budget), color = "black",position = "fill") + xlab("Fuel Type") + ylab("Budget Proportion")) 
(MPG_BP_fill <-ggplot(Vehicle, aes(MPG_C)) + geom_bar(aes(fill = Budget), color = "black",position = "fill") + xlab("MPG") + ylab("Budget Proportion"))
(Engine_BP_fill <-ggplot(Vehicle, aes(EngineSize_C)) + geom_bar(aes(fill = Budget), color = "black",position = "fill") + xlab("Engine Size") + ylab("Budget Proportion"))
(MA_BP_fill <-ggplot(Vehicle, aes(Mileage_C)) + geom_bar(aes(fill = Budget), color = "black",position = "fill") + xlab("Mileage") + ylab("Budget Proportion"))

