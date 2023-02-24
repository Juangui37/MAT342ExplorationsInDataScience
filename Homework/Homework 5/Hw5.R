a <- zoo
a <- a[,-1]

levels(a$class_type) <- c("Mammal", "Bird", "Reptile", "Fish", "Amphibian", "Bug", "Invertebrate")

a$class_type <- as.factor(a$class_type)

summary(a$class_type)

table <- table(a$class_type)
(41/(41+20+5+13+4+8+10))
library(caret)
set.seed(177)
nTrain <- createDataPartition(y = a$class_type, p =.75, list = FALSE)

a_Train <- a[nTrain,]

a_Test <- a[-nTrain,]

a_Train$trainortest <- rep("train", nrow(a_Train))
a_Test$trainortest <- rep("test", nrow(a_Test))
All_Data <- rbind(a_Train, a_Test)

#Boxplots
par(mfrow=c(2,3))

kruskal.test(predator ~ as.factor(trainortest), data = All_Data)
kruskal.test(aquatic ~ as.factor(trainortest), data = All_Data)
kruskal.test(a$milk ~ as.factor(trainortest), data = All_Data)

names(a_Train)
a_Train <- a_Train[,-18]
a_Test <- a_Train[,-18]
library(C50)
mod1 <- C5.0(class_type ~.,data = a_Train) #income = target variable... "~." = all data
summary(mod1)
plot(mod1)

mod1.test <- predict(object = mod1,newdata = a_Test)
table(a_Test$class_type,mod1.test)

dim(a_Test)[1]
74/77



