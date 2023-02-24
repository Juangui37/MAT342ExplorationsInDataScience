

churn_train$trainortest <- rep("train", nrow(churn_train))
churn_test$trainortest <- rep("test", nrow(churn_test))

c <- rbind(churn_test, churn_train)
summary(c)

#Graphical Displays
library(ggplot2)
library(gridExtra)

#Day Mins
a<- ggplot(c, aes(Day.Mins)) + geom_histogram(aes(fill = Churn.), color = "black", binwidth = 10, position = "stack") + xlab("Day Minutes") + ylab("Churn Proportion") + ggtitle("Stacked Histogram of Day Minutes with Overlay of Churn")
b<- ggplot(c, aes(Day.Mins)) + geom_histogram(aes(fill = Churn.), color = "black", binwidth = 10, position = "fill") + xlab("Day Minutes") + ylab("Churn Proportion") + ggtitle("Filled Histogram of Day Minutes with Overlay of Churn")
grid.arrange(a,b)

#International Plan
d <- ggplot(c, aes(c$Int.l.Plan)) + geom_bar(aes(fill = Churn.), color = "black", position = "stack") + xlab("International Plan") + ylab("Churn Proportion") + ggtitle("Stacked Histogram of International Plan with Overlay of Churn")
e <- ggplot(c, aes(c$Int.l.Plan)) + geom_bar(aes(fill = Churn.), color = "black", position = "fill") + xlab("International Plan") + ylab("Churn Proportion") + ggtitle("Filled Histogram of International Plan with Overlay of Churn")
grid.arrange(d,e)

#Voice Mail Plan
f <- ggplot(c, aes(c$VMail.Plan)) + geom_bar(aes(fill = Churn.), color = "black", position = "stack") + xlab("Voice Mail Plan") + ylab("Churn Proportion") + ggtitle("Stacked Histogram of Voice Mail Plan with Overlay of Churn")
g <- ggplot(c, aes(c$VMail.Plan)) + geom_bar(aes(fill = Churn.), color = "black", position = "fill") + xlab("Voice Mail Plan") + ylab("Churn Proportion") + ggtitle("Filled Histogram of Voice Mail Plan with Overlay of Churn")
grid.arrange(f,g)


h<- ggplot(c, aes(CustServ.Calls)) + geom_histogram(aes(fill = Churn.), color = "black", binwidth = 1, position = "stack") + xlab("Customer Service Calls") + ylab("Churn Proportion") + ggtitle("Stacked Histogram of Customer Service Calls with Overlay of Churn")
i<- ggplot(c, aes(CustServ.Calls)) + geom_histogram(aes(fill = Churn.), color = "black", binwidth = 1, position = "fill") + xlab("Customer Service Calls") + ylab("Churn Proportion") + ggtitle("Filled Histogram of Customer Service Calls with Overlay of Churn")
grid.arrange(h,i)

##Boxplots for Numeric Validation
set.seed(69)
par(mfrow=c(2,1))
boxplot(c$Day.Mins ~as.factor(trainortest), data = c, horizontal = TRUE, main = "Day Minutes Training & Test Boxplot", col = "blue")
boxplot(c$CustServ.Calls ~as.factor(trainortest), data = c, horizontal = TRUE, main = "Customer Service Calls Training & Test Boxplot", col = "blue")

kruskal.test(c$Day.Mins ~ as.factor(trainortest), data = c)$p.value
kruskal.test(c$CustServ.Calls ~ as.factor(trainortest), data = c)$p.value

#Tables for Categorical Validation
V_Table <- table(c$VMail.Plan, c$trainortest)

prop.test(V_Table, correct = FALSE)

I_Table <- table(c$Int.l.Plan, c$trainortest)

prop.test(I_Table, correct = FALSE)


#Baseline for our model
table(c$Churn.)
2850+483
2850/3333

#Removing testing an training data
names(churn_train)
churn_train<- churn_train[,-7]
names(churn_train)
names(churn_test)
churn_test<- churn_test[,-7]
names(churn_train)

library(C50)
mod1 <- C5.0(Churn. ~.,data = churn_train) #income = target variable... "~." = all data
summary(mod1)
plot(mod1)

mod1.test <- predict(object = mod1,newdata = churn_test)
table(churn_test$Churn.,mod1.test)

dim(churn_test)[1]





