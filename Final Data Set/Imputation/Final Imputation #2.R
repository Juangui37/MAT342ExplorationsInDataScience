#Use Packages: missForest, naniar, tidr, and mice

#We're going to make sure R reads N/A Values as "NA values" and not characters
#Use Dplyr Package to manipulate all the variable names to have no spaces
pay_back_NA <- degrees_that_pay_back
pay_back_NA[pay_back_NA == "N/A"] <- NA
pay_back_NA <- pay_back_NA %>% rename(Undergraduate_Major = `Undergraduate Major`,
                                      Starting_Median_Salary = `Starting Median Salary`,
                                      Mid_Career_Median_Salary = `Mid-Career Median Salary`,
                                      Mid_Career_10th_percentile_Salary =`Mid-Career 10th Percentile Salary`,
                                      Mid_Career_25th_percentile_Salary = `Mid-Career 25th Percentile Salary`,
                                      Mid_Career_75th_percentile_Salary = `Mid-Career 75th Percentile Salary`,
                                      Mid_Career_90th_percentile_Salary = `Mid-Career 90th Percentile Salary`)
colSums(is.na(pay_back_NA)) #No missing values

college_type_NA <- salaries_by_college_type
college_type_NA[college_type_NA == "N/A"] <- NA
college_type_NA <- college_type_NA %>% rename(School_Name = `School Name`,
                                              School_Type = `School Type`,
                                              Starting_Median_Salary = `Starting Median Salary`,
                                              Mid_Career_Median_Salary = `Mid-Career Median Salary`,
                                              Mid_Career_10th_percentile_Salary =`Mid-Career 10th Percentile Salary`,
                                              Mid_Career_25th_percentile_Salary = `Mid-Career 25th Percentile Salary`,
                                              Mid_Career_75th_percentile_Salary = `Mid-Career 75th Percentile Salary`,
                                              Mid_Career_90th_percentile_Salary = `Mid-Career 90th Percentile Salary`)
colSums(is.na(college_type_NA)) # 38 Missing values in 10th & 90th percentile

region_NA <- salaries_by_region
region_NA[region_NA == "N/A"] <- NA
region_NA <- region_NA %>% rename(School_Name = `School Name`,
                                  Region = `Region`,
                                  Starting_Median_Salary = `Starting Median Salary`,
                                  Mid_Career_Median_Salary = `Mid-Career Median Salary`,
                                  Mid_Career_10th_percentile_Salary =`Mid-Career 10th Percentile Salary`,
                                  Mid_Career_25th_percentile_Salary = `Mid-Career 25th Percentile Salary`,
                                  Mid_Career_75th_percentile_Salary = `Mid-Career 75th Percentile Salary`,
                                  Mid_Career_90th_percentile_Salary = `Mid-Career 90th Percentile Salary`)
colSums(is.na(region_NA)) # 47 Missing Values in 10th & 90th Percentile


#Now we'll use mice (I'm following along on a website)
summary(pay_back_NA)
summary(region_NA) #R reads: school name + region & 10th + 90th percentile variables as characters
summary(college_type_NA)#R reads: school name + type & 10th + 90th percentile variables as characters


class(college_type_NA$Mid_Career_10th_percentile_Salary)
class(college_type_NA$Mid_Career_90th_percentile_Salary)
class(college_type_NA$`School Name`)
class(college_type_NA$`School Type`)

#TWe're going to change this to characters to numeric 
college_type_NA$Mid_Career_10th_percentile_Salary <- 
  as.numeric(college_type_NA$Mid_Career_10th_percentile_Salary)
college_type_NA$Mid_Career_90th_percentile_Salary <- 
  as.numeric(college_type_NA$Mid_Career_90th_percentile_Salary)

region_NA$Mid_Career_10th_percentile_Salary <- as.numeric(region_NA$Mid_Career_10th_percentile_Salary)
region_NA$Mid_Career_90th_percentile_Salary <- as.numeric(region_NA$Mid_Career_90th_percentile_Salary)

#And characters to factors
college_type_NA$School_Name <- as.factor(college_type_NA$School_Name) 
college_type_NA$School_Type` <- as.factor(college_type_NA$School_Type)
region_NA$School_Name <- as.factor(region_NA$School_Name)
region_NA$Region <- as.factor(region_NA$Region)

#For some reason R still thinks that 90th percentile is character but, it doesn't display it as that on the summary.


#*IGNORE THIS FOR NOW*
#Now, following the website, We're removing categorical variables to focus on the numeric varaibles
#college_type_NA <- subset(college_type_NA, select = -c(`School Name`, `School Type`))
#*IGNORE THIS FOR NOW


#Now we're going to use mice package to show us a tabular form of missing value present in each variable in a data set.
md.pattern(region_NA)

#Next we'll use the VIM package to create a visual of what Mice made
mice_plot <- aggr(region_NA, col=c('navyblue','yellow'),
                  numbers=TRUE, sortVars=TRUE,
                  labels = names(region_NA), cex.axis=.7,
                  gap=3, ylab = c("Missing data","Pattern"))



#Now we impute the missing values
imputed_Data <- mice(region_NA, m=5, maxit = 50, method = 'cart', seed = 89)
imputed_Data$imp$School_Name

imputed_Data$imp$Mid_Career_10th_percentile_Salary
imputed_Data$method

#Our 'imputed_Data' variable has 5 different imputed data sets. (because of m = 5 in our equation) 
#We can check out the 5 different data sets with imputed info like this:
imputed_Data_1 <- complete(imputed_Data,1)
imputed_Data_2 <- complete(imputed_Data,2)
imputed_Data_3 <- complete(imputed_Data,3)
imputed_Data_4 <- complete(imputed_Data,4)
imputed_Data_5 <- complete(imputed_Data,5)

imputed_Data_1
imputed_Data_2
imputed_Data_3
imputed_Data_4
imputed_Data_5

#Now lets build models on all 5 imputed datasets

#build predictive model
fit <- with(data = region_NA, lm(Starting_Median_Salary +
                                         Mid_Career_Median_Salary +
                                         Mid_Career_10th_percentile_Salary +
                                         Mid_Career_25th_percentile_Salary +
                                         Mid_Career_75th_percentile_Salary +
                                         Mid_Career_90th_percentile_Salary ~ Region))


summary(pool(fit)) 

pool(fit)

completedata <- pool(fit)

completedata$g
