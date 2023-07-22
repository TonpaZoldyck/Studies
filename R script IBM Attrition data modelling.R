#Understanding data summary
summary(ibmtrain)

attach(ibmtrain)


#looking at the values of all variables
#Making attrition binary
Attrition2<- ifelse(Attrition=="Yes", 1,0)
ibmtrain$Attrition2<-Attrition2



#
ibmtrain_copy$BusinessTravelnum[ibmtrain_copy$BusinessTravel == "Non-Travel" ] <- 0


--------------------------------------------------
  #Data description: Looking at numeric variables
--------------------------------------------------
# Plots of numerical variables vs attrition
par(mfrow=c(2,2))
boxplot(HourlyRate~Attrition, xlab="Attrition", ylab= "HourlyRate", Main= "HourlyRate against Attrition")
boxplot(MonthlyRate~Attrition,outline = F, xlab="Attrition", ylab= "MonthlyRate", Main= "MonthlyRate against Attrition")
boxplot(NumCompaniesWorked~Attrition, xlab="Attrition", ylab= "Number of companies", Main= "Number of companies against Attrition")
boxplot(YearsSinceLastPromotion~Attrition,outline = F, xlab="Attrition", ylab= "Years Since Last Promotion", Main= "Years Since Last Promotion against Attrition")


#variables with signifcant differences
par(mfrow=c(2,2))
boxplot(Age~Attrition, xlab="Attrition", ylab= "Age", Main= "Age against Attrition")
boxplot(DailyRate~Attrition, xlab="Attrition", ylab= "DailyRate", Main= "DailyRate against Attrition")
boxplot(MonthlyIncome~Attrition,outline = F, xlab="Attrition", ylab= "MonthlyIncome", Main= "MonthlyIncome against Attrition")
boxplot(DistanceFromHome~Attrition, xlab="Attrition", ylab= "DistanceFromHome", Main= "DistanceFromHome against Attrition")
boxplot(YearsAtCompany~Attrition,outline=F, xlab="Attrition", ylab= "Years At Company", Main= "Years At Company against Attrition")
boxplot(YearsInCurrentRole~Attrition, outline = F, xlab="Attrition", ylab= "Years in Current Role", Main= "Years in Current Role against Attrition")
boxplot(YearsWithCurrManager~Attrition,outline = F, xlab="Attrition", ylab= "Years With Current Manager", Main= "Years With current Manager against Attrition")
boxplot(TotalWorkingYears~Attrition,outline = F, xlab="Attrition", ylab= "Total working years")


#how money affects attrition
boxplot(MonthlyIncome~Attrition,outline = F, xlab="Attrition", ylab= "MonthlyIncome", Main= "MonthlyIncome against Attrition")
boxplot(HourlyRate~Attrition, xlab="Attrition", ylab= "HourlyRate", Main= "HourlyRate against Attrition")
boxplot(MonthlyRate~Attrition,outline = F, xlab="Attrition", ylab= "MonthlyRate", Main= "MonthlyRate against Attrition")
boxplot(DailyRate~Attrition, xlab="Attrition", ylab= "DailyRate", Main= "DailyRate against Attrition")

# testing the significance of difference means for attrition yes vs no, sig level 0.05
#t test age
Age1 = Age[Attrition2==1]
Age2 = Age[Attrition2==0]
t.test(Age1, Age2)
# SIGNIFICANT p-Value = 4.677e-08

#t test daily rate
DailyRate1 = DailyRate[Attrition2==1]
DailyRate0 = DailyRate[Attrition2==0]

t.test(DailyRate0, DailyRate1)
# p-value = 0.02836
#t test distance from home
dishome1 = DistanceFromHome[Attrition2==1]
dishome2 = DistanceFromHome[Attrition2==0]

t.test(dishome1, dishome2)
# SIGNIFICANT p-Value =  0.03358


#t test 
monin1 = MonthlyIncome[Attrition2==1]
monin2 = MonthlyIncome[Attrition2==0]
t.test(monin1, monin2)
# SIGNIFICANT p-Value = 1.124e-11

#t test 
twy1 = TotalWorkingYears[Attrition2==1]
twy2 = TotalWorkingYears[Attrition2==0]
t.test(twy1, twy2)
# SIGNIFICANT p-Value = 1.699e-10

#t test 
yac1 = YearsAtCompany[Attrition2==1]
yac2 = YearsAtCompany[Attrition2==0]
t.test(yac1, yac2)
# SIGNIFICANT p-Value = 3.683e-06
#t test 
yicr1 = YearsInCurrentRole[Attrition2==1]
yicr2 = YearsInCurrentRole[Attrition2==0]
t.test(yicr1, yicr2)
# SIGNIFICANT p-Value = 3.882e-08
#t test 
ywcm1 = YearsWithCurrManager[Attrition2==1]
ywcm2 = YearsWithCurrManager[Attrition2==0]
t.test(ywcm1, ywcm2)
# SIGNIFICANT p-Value = 2.939e-08

#t test 
yslp1 = YearsSinceLastPromotion[Attrition2==1]
yslp2 = YearsSinceLastPromotion[Attrition2==0]
t.test(yslp1, yslp2)
# INSIGNIFICANT p-Value = 0.5375
#t test 
ncwa1 = NumCompaniesWorked[Attrition2==1]
ncwa2 = NumCompaniesWorked[Attrition2==0]
t.test(ncwa1, ncwa2)
# INSIGNIFICANT p-Value = 0.1053
#t test 
monrate1 = MonthlyRate[Attrition2==1]
monrate2 = MonthlyRate[Attrition2==0]
t.test(monrate1, monrate2)
# INSIGNIFICANT p-Value = 0.583
#t test 
hourlyrate1 = HourlyRate[Attrition2==1]
hourlyrate2 = HourlyRate[Attrition2==0]
t.test(hourlyrate1, hourlyrate2)
# INSIGNIFICANT p-Value = 0.5715
#insignificant variables:
#hourlyrate
#monthlyrate
#numcompnaies
#years since last promotion

# Looking at non - numeric against attrition
prop.table(table(BusinessTravel,Attrition))
plot(prop.table(table(BusinessTravel,Attrition)), cex = 0.5)
#
prop.table(table(Department,Attrition))
pieplot(prop.table(table(Department,Attrition)), cex = 0.5)











#creating and testing models
library(class)
library(caret)
k = 5
folds = createFolds(ibmtrain$Attrition, k, list = TRUE, returnTrain = FALSE)
accuracy = double(k)
for(i in 1:k){
train = ibmtrain[-folds[[i]],]
test = ibmtrain[folds[[i]],]
mdl =glm(Attrition ~.Age+ DailyRate+ MonthlyIncome+DistanceFromHome+YearsAtCompany+YearsWithCurrManager+ YearsInCurrentRole,  data=train)
predict = predict(tree,newdata=test)
accuracy[i] = mean(predict==test$Attrition)}



## finding importance of variables
#packages need, bindrcpp, ggplot2, modeltools, dplyr, DMwr, caret, plyr, xgboost, rminer, gridExtra
library(bindrcpp)
library(ggplot2)
library(modeltools)
library(dplyr)
library(DMwR)
library(caret)
library(plyr)
library(xgboost)
library(gridExtra)








ibmtrain<-ibmtrain_Copy

set.seed(123)
xgbData <- ibmtrain
indexes <- sample(1:nrow(xgbData), size=0.8*nrow(xgbData))
XGBtrain.Data <- xgbData[indexes,]
XGBtest.Data <- xgbData[-indexes,]

formula = Attrition~.
fitControl <- trainControl(method="cv", number = 3,classProbs = TRUE )
xgbGrid <- expand.grid(nrounds = 50,
                       max_depth = 12,
                       eta = .03,
                       gamma = 0.01,
                       colsample_bytree = .7,
                       min_child_weight = 1,
                       subsample = 0.9
)
XGB.model <- train(formula, data = XGBtrain.Data,
                   method = "xgbTree"
                   ,trControl = fitControl
                   , verbose=0
                   , maximize=FALSE
                   ,tuneGrid = xgbGrid
)
importance <- varImp(XGB.model)
varImportance <- data.frame(Variables = row.names(importance[[1]]), 
                            Importance = round(importance[[1]]$Overall,2))
# Create a rank variable based on importance of variables
rankImportance <- varImportance %>%
  mutate(Rank = paste0('#',dense_rank(desc(Importance))))
ggplot(rankImportance, aes(x = reorder(Variables, Importance), 
                           y = Importance)) +
  geom_bar(stat='identity',colour="white", fill = "lightgreen") +
  geom_text(aes(x = Variables, y = 1, label = Rank, size = 1),
            hjust=0, vjust=.5, size = 2.5, colour = 'black',
            fontface = 'bold') +
  labs(x = 'Variables', title = 'Relative Variable Importance') +
  coord_flip() + 
  theme_bw()

----------------------
# CHoosing variables 
----------------------
# After plots a tree with all the variables we ran a glm with the main most important variables from the tree
#logistic regression glm
  k = 10
folds = createFolds(ibmtrain$Attrition, k, list = TRUE, returnTrain = FALSE)
accuracy = double(k)
for(i in 1:k){
  train = ibmtrain[-folds[[i]],]
  test = ibmtrain[folds[[i]],]
  regression = glm(as.factor(Attrition) ~ Age+BusinessTravel+DailyRate+Department+DistanceFromHome+Education+EducationField+EnvironmentSatisfaction+Gender+HourlyRate+JobInvolvement+JobLevel+JobRole+JobSatisfaction+MaritalStatus+MonthlyIncome+MonthlyRate+NumCompaniesWorked+OverTime+PercentSalaryHike+PerformanceRating+RelationshipSatisfaction+StockOptionLevel+TrainingTimesLastYear, data=train, family="binomial")
  probs = predict(regression,newdata=test,type="response")
  predict = ifelse(probs>.5, "Yes", "No")
  accuracy[i] = mean(predict==test$Attrition)
}
accuracy
mean(accuracy)
###
#the model
tree = rpart(as.factor(Attrition) ~ Age+BusinessTravel+DailyRate+Department+DistanceFromHome+Education+EducationField+EnvironmentSatisfaction+Gender+HourlyRate+JobInvolvement+JobLevel+JobRole+JobSatisfaction+MaritalStatus+MonthlyIncome+MonthlyRate+NumCompaniesWorked+OverTime+PercentSalaryHike+PerformanceRating+RelationshipSatisfaction+StockOptionLevel+TrainingTimesLastYear,data=train, method="class")

k = 10
folds = createFolds(ibmtrain$Attrition, k, list = TRUE, returnTrain = FALSE)
accuracy = double(k)
for(i in 1:k){
  train = ibmtrain[-folds[[i]],]
  test = ibmtrain[folds[[i]],]
  tree = rpart(as.factor(Attrition) ~ Age+BusinessTravel+DailyRate+Department+DistanceFromHome+Education+EducationField+EnvironmentSatisfaction+Gender+HourlyRate+JobInvolvement+JobLevel+JobRole+JobSatisfaction+MaritalStatus+MonthlyIncome+MonthlyRate+NumCompaniesWorked+OverTime+PercentSalaryHike+PerformanceRating+RelationshipSatisfaction+StockOptionLevel+TrainingTimesLastYear,data=train, method="class")
  predict = predict(tree,newdata=test,type="class")
  accuracy[i] = mean(predict==test$Attrition)
}
accuracy
mean(accuracy)
### 0.8614733
##
k = 10
folds = createFolds(ibmtrain$Attrition, k, list = TRUE, returnTrain = FALSE)
accuracy = double(k)
for(i in 1:k){
  train = ibmtrain[-folds[[i]],]
  test = ibmtrain[folds[[i]],]
  tree = rpart(as.factor(Attrition) ~ Age+BusinessTravel+DailyRate+Department+DistanceFromHome+Education+EducationField+EnvironmentSatisfaction+Gender+HourlyRate+JobInvolvement+JobLevel+JobRole+JobSatisfaction+MaritalStatus+MonthlyIncome+MonthlyRate+NumCompaniesWorked+OverTime+PercentSalaryHike+PerformanceRating+RelationshipSatisfaction+StockOptionLevel+TrainingTimesLastYear,data=train, method="class")
  predict = predict(tree,newdata=test,type="class")
  accuracy[i] = mean(predict==test$Attrition)
}
accuracy
mean(accuracy)
#Plotting the graph

tree_plot = plot(tree, uniform = T)
text(tree_plot)
###???
treeall = rpart(as.factor(Attrition)~ Age+BusinessTravel+DailyRate+Department+DistanceFromHome+Education+EducationField+EnvironmentSatisfaction+Gender+HourlyRate+JobInvolvement+JobLevel+JobRole+JobSatisfaction+MaritalStatus+MonthlyIncome+MonthlyRate+NumCompaniesWorked+OverTime+PercentSalaryHike+PerformanceRating+RelationshipSatisfaction+StockOptionLevel+TotalWorkingYears+TrainingTimesLastYear+WorkLifeBalance+YearsAtCompany+YearsInCurrentRole+YearsSinceLastPromotion+YearsWithCurrManager ,  data=train, method = "class")
treefake = rpart(as.factor(Attrition)~ Age+BusinessTravel+DailyRate+Department+DistanceFromHome+Education+EducationField+EnvironmentSatisfaction+Gender+HourlyRate+JobInvolvement+JobLevel+JobRole+JobSatisfaction+MaritalStatus+MonthlyIncome+MonthlyRate+NumCompaniesWorked+OverTime+PercentSalaryHike+PerformanceRating+RelationshipSatisfaction+StockOptionLevel+TotalWorkingYears+TrainingTimesLastYear+WorkLifeBalance+YearsAtCompany+YearsInCurrentRole+YearsSinceLastPromotion+YearsWithCurrManager ,  data=train, method = "anova")

---------------------
#Logistic regression
---------------------
#Model with all variables
regression1 = glm(as.factor(Attrition)~ Age+BusinessTravel+DailyRate+Department+DistanceFromHome+Education+EducationField+EnvironmentSatisfaction+Gender+HourlyRate+JobInvolvement+JobLevel+JobRole+JobSatisfaction+MaritalStatus+MonthlyIncome+MonthlyRate+NumCompaniesWorked+OverTime+PercentSalaryHike+PerformanceRating+RelationshipSatisfaction+StockOptionLevel+TotalWorkingYears+TrainingTimesLastYear+WorkLifeBalance+YearsAtCompany+YearsInCurrentRole+YearsSinceLastPromotion+YearsWithCurrManager ,  data=train, family="binomial")
k = 10
folds = createFolds(ibmtrain$Attrition, k, list = TRUE, returnTrain = FALSE)
accuracy = double(k)
for(i in 1:k){
  train = ibmtrain[-folds[[i]],]
  test = ibmtrain[folds[[i]],]
  regression1 = glm(as.factor(Attrition)~ Age+BusinessTravel+DailyRate+Department+DistanceFromHome+Education+EducationField+EnvironmentSatisfaction+Gender+HourlyRate+JobInvolvement+JobLevel+JobRole+JobSatisfaction+MaritalStatus+MonthlyIncome+MonthlyRate+NumCompaniesWorked+OverTime+PercentSalaryHike+PerformanceRating+RelationshipSatisfaction+StockOptionLevel+TotalWorkingYears+TrainingTimesLastYear+WorkLifeBalance+YearsAtCompany+YearsInCurrentRole+YearsSinceLastPromotion+YearsWithCurrManager ,  data=train, family="binomial")
  probs = predict(regression,newdata=test,type="response")
  predict = ifelse(probs>.5, "Yes", "No")
  accuracy[i] = mean(predict==test$Attrition)
}
accuracy
mean(accuracy)
####0.8810445

### model with only the significant variables

regression2 = glm(as.factor(Attrition)~ BusinessTravel+DistanceFromHome+EnvironmentSatisfaction+JobInvolvement+JobSatisfaction+NumCompaniesWorked+OverTime+RelationshipSatisfaction+YearsAtCompany+YearsInCurrentRole+YearsSinceLastPromotion+YearsWithCurrManager ,  data=train, family="binomial")
k = 10
folds = createFolds(ibmtrain$Attrition, k, list = TRUE, returnTrain = FALSE)
accuracy = double(k)
for(i in 1:k){
  train = ibmtrain[-folds[[i]],]
  test = ibmtrain[folds[[i]],]
  regression2 = glm(as.factor(Attrition)~ BusinessTravel+DistanceFromHome+EnvironmentSatisfaction+JobInvolvement+JobSatisfaction+NumCompaniesWorked+OverTime+RelationshipSatisfaction+YearsAtCompany+YearsInCurrentRole+YearsSinceLastPromotion+YearsWithCurrManager ,  data=train, family="binomial")
  probs = predict(regression2,newdata=test,type="response")
  predict = ifelse(probs>.5, "Yes", "No")
  accuracy[i] = mean(predict==test$Attrition)
}
accuracy
mean(accuracy)
### mean(accuracy)= 0.8512965


#### Next model test
regression3 = glm(as.factor(Attrition)~ BusinessTravel+DistanceFromHome+EnvironmentSatisfaction+JobInvolvement+JobSatisfaction+NumCompaniesWorked+RelationshipSatisfaction+YearsInCurrentRole+YearsSinceLastPromotion ,  data=train, family="binomial")
k = 10
folds = createFolds(ibmtrain$Attrition, k, list = TRUE, returnTrain = FALSE)
accuracy = double(k)
for(i in 1:k){
  train = ibmtrain[-folds[[i]],]
  test = ibmtrain[folds[[i]],]
  regression3 = glm(as.factor(Attrition)~ BusinessTravel+DistanceFromHome+EnvironmentSatisfaction+JobInvolvement+JobSatisfaction+NumCompaniesWorked+RelationshipSatisfaction+YearsInCurrentRole+YearsSinceLastPromotion ,  data=train, family="binomial")
  probs = predict(regression3,newdata=test,type="response")
  predict = ifelse(probs>.5, "Yes", "No")
  accuracy[i] = mean(predict==test$Attrition)
}
accuracy
mean(accuracy)
#### 0.8436911
#### next model removing insignificant variables
regression4 = glm(as.factor(Attrition)~ BusinessTravel+DistanceFromHome+EnvironmentSatisfaction+JobInvolvement+JobSatisfaction+YearsInCurrentRole+YearsSinceLastPromotion ,  data=train, family="binomial")
k = 10
folds = createFolds(ibmtrain$Attrition, k, list = TRUE, returnTrain = FALSE)
accuracy = double(k)
for(i in 1:k){
  train = ibmtrain[-folds[[i]],]
  test = ibmtrain[folds[[i]],]
  regression4 = glm(as.factor(Attrition)~ BusinessTravel+DistanceFromHome+EnvironmentSatisfaction+JobInvolvement+JobSatisfaction+YearsInCurrentRole+YearsSinceLastPromotion ,  data=train, family="binomial")
  probs = predict(regression4,newdata=test,type="response")
  predict = ifelse(probs>.5, "Yes", "No")
  accuracy[i] = mean(predict==test$Attrition)
}
accuracy
mean(accuracy)
#### 0.8428002
#At this point all variables are significant

#Looking through the data key variables
# Age, OT, Monthlyincome, yearsatcom, business travel
k = 10
folds = createFolds(ibmtrain$Attrition, k, list = TRUE, returnTrain = FALSE)
accuracy = double(k)
for(i in 1:k){
  train = ibmtrain[-folds[[i]],]
  test = ibmtrain[folds[[i]],]
  regression6 = glm(as.factor(Attrition)~ OverTime + MonthlyIncome + YearsAtCompany + NumCompaniesWorked + TotalWorkingYears + I(Age^2) ,  data=train, family="binomial")
  probs = predict(regression6,newdata=test,type="response")
  predict = ifelse(probs>.5, "Yes", "No")
  accuracy[i] = mean(predict==test$Attrition)
}
accuracy
mean(accuracy)


------------------------------
#Random forest model
------------------------------
  k = 10
folds = createFolds(ibmtrain$Attrition, k, list = TRUE, returnTrain = FALSE)
accuracy = double(k)
for(i in 1:k){
  train = ibmtrain[-folds[[i]],]
  test = ibmtrain[folds[[i]],]
  forest = randomForest(as.factor(Attrition) ~ Age+BusinessTravel+DailyRate+Department+DistanceFromHome+Education+EducationField+EnvironmentSatisfaction+Gender+HourlyRate+JobInvolvement+JobLevel+JobRole+JobSatisfaction+MaritalStatus+MonthlyIncome+MonthlyRate+NumCompaniesWorked+OverTime+PercentSalaryHike+PerformanceRating+RelationshipSatisfaction+StockOptionLevel+TotalWorkingYears+TrainingTimesLastYear+WorkLifeBalance+YearsAtCompany+YearsInCurrentRole+YearsSinceLastPromotion+YearsWithCurrManager, data=train, ntree = 200, importance = TRUE)
  predict = predict(forest,newdata=test)
  accuracy[i] = mean(predict==test$Attrition)
}
accuracy
mean(accuracy)

#### next random forest model
k = 10
folds = createFolds(ibmtrain$Attrition, k, list = TRUE, returnTrain = FALSE)
accuracy = double(k)
for(i in 1:k){
  train = ibmtrain[-folds[[i]],]
  test = ibmtrain[folds[[i]],]
  forest = randomForest(as.factor(Attrition) ~ Age+MonthlyIncome+OverTime, data=train, ntree = 200, importance = TRUE)
  predict = predict(forest,newdata=test)
  accuracy[i] = mean(predict==test$Attrition)
}
accuracy
mean(accuracy)

###moving out of r
stargazer(regression9, type = "text")


#Making models
#doing as.factor overall. Only need to do this step once
ibmtrain$BusinessTravel = as.factor(ibmtrain$BusinessTravel)
ibmtrain$Department = as.factor(ibmtrain$Department)
ibmtrain$EducationField = as.factor(ibmtrain$EducationField)
ibmtrain$Gender = as.factor(ibmtrain$Gender)
ibmtrain$JobRole = as.factor(ibmtrain$JobRole)
ibmtrain$MaritalStatus = as.factor(ibmtrain$MaritalStatus)
ibmtrain$OverTime = as.factor(ibmtrain$OverTime)

#### 6 chosen variables from data description
regression9 = glm(as.factor(Attrition)~Age + MonthlyIncome + YearsAtCompany+NumCompaniesWorked+TotalWorkingYears+OverTime, data = train, family = "binomial")
tree9 = rpart(as.factor(Attrition)~Age + MonthlyIncome + YearsAtCompany+NumCompaniesWorked+TotalWorkingYears+OverTime, data = train,method = "class" )
forest1 = randomForest(as.factor(Attrition)~Age + MonthlyIncome + YearsAtCompany+NumCompaniesWorked+TotalWorkingYears+OverTime, data = train, ntree = 200, importance = TRUE )
forest = randomForest(as.factor(Attrition) ~ Age+MonthlyIncome+OverTime, data=train, ntree = 200, importance = TRUE)
####predict
regression9 = glm(as.factor(Attrition)~Age + MonthlyIncome + TotalWorkingYears+NumCompaniesWorked+TotalWorkingYears+OverTime, data = train, family = "binomial")
k = 10
folds = createFolds(ibmtrain$Attrition, k, list = TRUE, returnTrain = FALSE)
accuracy = double(k)
for(i in 1:k){
  train = ibmtrain[-folds[[i]],]
  test = ibmtrain[folds[[i]],]
  regression9 = glm(as.factor(Attrition)~Age + MonthlyIncome + TotalWorkingYears+NumCompaniesWorked+TotalWorkingYears+OverTime+ HourlyRate+StockOptionLevel+DailyRate, data = train, family = "binomial")
  probs = predict(regression4,newdata=test,type="response")
  predict = ifelse(probs>.5, "Yes", "No")
  accuracy[i] = mean(predict==test$Attrition)
}
accuracy
mean(accuracy)
###extras
##
#submitting to kaggle
model2 <- glm(as.factor(Attrition) ~ Age + TotalWorkingYears + TrainingTimesLastYear + WorkLifeBalance + YearsAtCompany + YearsInCurrentRole + YearsSinceLastPromotion + YearsWithCurrManager, data=train, family="binomial")
regression4 = glm(as.factor(Attrition)~ BusinessTravel+DistanceFromHome+EnvironmentSatisfaction+JobInvolvement+JobSatisfaction+YearsInCurrentRole+YearsSinceLastPromotion ,  data=train, family="binomial")
regression3 = glm(as.factor(Attrition)~ BusinessTravel+DistanceFromHome+EnvironmentSatisfaction+JobInvolvement+JobSatisfaction+NumCompaniesWorked+RelationshipSatisfaction+YearsInCurrentRole+YearsSinceLastPromotion ,  data=train, family="binomial")
regression2 = glm(as.factor(Attrition)~ BusinessTravel+DistanceFromHome+EnvironmentSatisfaction+JobInvolvement+JobSatisfaction+NumCompaniesWorked+OverTime+RelationshipSatisfaction+YearsAtCompany+YearsInCurrentRole+YearsSinceLastPromotion+YearsWithCurrManager ,  data=train, family="binomial")
regression1 = glm(as.factor(Attrition)~ Age+BusinessTravel+DailyRate+Department+DistanceFromHome+Education+EducationField+EnvironmentSatisfaction+Gender+HourlyRate+JobInvolvement+JobLevel+JobRole+JobSatisfaction+MaritalStatus+MonthlyIncome+MonthlyRate+NumCompaniesWorked+OverTime+PercentSalaryHike+PerformanceRating+RelationshipSatisfaction+StockOptionLevel+TotalWorkingYears+TrainingTimesLastYear+WorkLifeBalance+YearsAtCompany+YearsInCurrentRole+YearsSinceLastPromotion+YearsWithCurrManager ,  data=train, family="binomial")

prediction2 <- predict(regression1, type = "response", newdata = ibmtest)
glmprediction2 = ifelse(prediction2>.5, "Yes", "No")
solution2 <- data.frame(ID = ibmtest$ID, Attrition = glmprediction2)
write.csv(solution2, file = "solution5new_.csv", row.names=F)

#to submit the model, change the part in red to your own model

str(ibmtrain)
### importing the output out of r
stargazer(regression, type = "text")

#Exproting regression output into word
install.packages(stargazer)
