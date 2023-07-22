bank.df <- Bank_Personal_Loan_Modelling_1_

colnames(bank.df) <- c("ID","Age", "Experience", "Income", "ZIPCode", "FamilySize", "CreditCardSpend","EducationLevel", "Mortgage", 
                       "PersonalLoan", "SecuritiesAccount", "CertificateOfDeposit", "Online", "CreditCard")
smp_size <- floor(0.75 * nrow(bank.df))

train_ind <- sample(seq_len(nrow(bank.df)), size = smp_size)

banktrain <- bank.df[train_ind, ]
banktest <- bank.df[-train_ind, ]


###SupportVector Machine learning model:
set.seed(123)
mod.svm <- train(as.factor(PersonalLoan) ~Income + FamilySize +  CreditCardSpend+ EducationLevel+  
                   CertificateOfDeposit+  Online, method = "svmRadial",data = banktrain)
pred.svm <- predict(mod.svm, banktest)
confusionMatrix(pred.svm, banktest$PersonalLoan)$overall[1]

#### Classification Tree Model
Tree1 <- rpart(as.factor(PersonalLoan) ~Income + FamilySize +  CreditCardSpend+ EducationLevel+  
                 CertificateOfDeposit+  Online, data = banktrain, method = "class")
Pred.Tree <- predict(Tree1, banktest)
### Logistic Regression Model
Glm2 <- glm(as.factor(PersonalLoan) ~  Income +  CreditCardSpend+ EducationLevel+CertificateOfDeposit+  
              Online, data = banktrain, family = "binomial")

results <- resamples(list(mod1 = Tree1, mod2 = mod.svm))
modelCor(results)

nsamples_class <- 625
predDF <- data.frame(Pred.Tree, pred.svm, class = banktest$PersonalLoan)
predDF_bc <- undersample_ds(predDF, "class", nsamples_class)


### function
undersample_ds <- function(x, classCol, nsamples_class){
  for (i in 1:length(unique(x[, classCol]))){
    class.i <- unique(x[, classCol])[i]
    if((sum(x[, classCol] == class.i) - nsamples_class) != 0){
      x <- x[-sample(which(x[, classCol] == class.i), 
                     sum(x[, classCol] == class.i) - nsamples_class), ]
    }
  }
  return(x)
}
###########################
# Example of Stacking algorithms
# create submodels
control <- trainControl(method="repeatedcv", number=10, repeats=3, savePredictions=TRUE, classProbs=TRUE)
algorithmList <- c( 'rpart', 'glm', 'knn', 'svmRadial')
set.seed(123)
models <- caretList(PersonalLoan~Income + FamilySize +  CreditCardSpend+ as.factor(EducationLevel)+  
                      CertificateOfDeposit+  as.factor(Online), data=banktrain, trControl=control, methodList=algorithmList)
results <- resamples(models)
summary(results)
dotplot(results)
# correlation between results
modelCor(results)
splom(results)

stackControl <- trainControl(method="repeatedcv", number=10, repeats=3, savePredictions=TRUE, classProbs=TRUE)
set.seed(123)
stack.glm <- caretStack(models, method="glm", metric="Accuracy", trControl=stackControl)
print(stack.glm)


# stack using random forest
set.seed(seed)
stack.rf <- caretStack(models, method="rf", metric="Accuracy", trControl=stackControl)
print(stack.rf)
1
2
3
4
# stack using random forest
set.seed(123)
stack.rf <- caretStack(models, method="rf", metric="Accuracy", trControl=stackControl)
print(stack.rf)




#### Ensemble Learning Method Code
colnames(bank.df) <- c("ID","Age", "Experience", "Income", "ZIPCode", "FamilySize", "CreditCardSpend","EducationLevel", "Mortgage", 
                       "PersonalLoan", "SecuritiesAccount", "CertificateOfDeposit", "Online", "CreditCard")
str(bank.df)
bank.df$FamilySize <- as.factor(bank.df$FamilySize)
bank.df$EducationLevel <- as.factor(bank.df$EducationLevel)
bank.df$PersonalLoan <- as.factor(bank.df$PersonalLoan)
bank.df$SecuritiesAccount <- as.factor(bank.df$SecuritiesAccount)
bank.df$CertificateOfDeposit <- as.factor(bank.df$CertificateOfDeposit)
bank.df$Online <- as.factor(bank.df$Online)
bank.df$CreditCard <- as.factor(bank.df$CreditCard)
bank.df$Experience <- as.numeric(bank.df$Experience)
bank.df$Income <- as.numeric(bank.df$Income)
bank.df$Age <- as.numeric(bank.df$Age)
bank.df$Mortgage <- as.numeric(bank.df$Mortgage)
bank.df$ZIPCode <- as.character(bank.df$ZIPCode)
str(bank.df)
bank.df <- data.frame(lapply(bank.df, as.character), stringsAsFactors=FALSE)
bank.df <- data.frame(lapply(bank.df, as.numeric))

#### creating data splits:
bank.df1 <- select(bank.df,PersonalLoan, Income, FamilySize, CreditCardSpend, EducationLevel,
                     CertificateOfDeposit,  Online )

set.seed(123)
bank.df1 <- bank.df1[sample(nrow(bank.df)),]
split <- floor(nrow(bank.df)/3)
trainingData <- bank.df1[0:split,]
validationData <- bank.df1[(split+1):(split*2),]
testingData <- bank.df[(split*2+1):nrow(bank.df),]

dim(trainingData)
dim(validationData)
dim(testingData)

labelName <- 'PersonalLoan'

predictors <- names(trainingData)[names(trainingData) != labelName]

myControl <- trainControl(method='cv', number=3, returnResamp='none')


test_model1 <- train(validationData[,predictors], validationData[,labelName], method='glm', trControl=myControl)
test_model2 <- train(validationData[,predictors], validationData[,labelName], method='svmRadial', trControl=myControl)
test_model <- train(validationData[,predictors], validationData[,labelName], method='rpart', trControl=myControl)

###Accuracy
preds <- predict(object=test_model, testingData[,predictors])
library(pROC)
auc <- roc(testingData[,labelName], preds)
print(auc$auc) 
model_tree <- train(trainingData[,predictors], trainingData[,labelName], method='rpart', trControl=myControl)

model_glm <- train(trainingData[,predictors], trainingData[,as.factor(labelName)], method='glm', trControl=myControl)

model_svm <- train(trainingData[,predictors], trainingData[,labelName], method='svmRadial', trControl=myControl)


validationData$tree_PROB <- predict(object=model_tree, validationData[,predictors])
validationData$glm_PROB <- predict(object=model_glm, validationData[,predictors])
validationData$svm_PROB <- predict(object=model_svm, validationData[,predictors])

testingData$tree_PROB <- predict(object=model_tree, testingData[,predictors])
testingData$glm_PROB <- predict(object=model_glm, testingData[,predictors])
testingData$svm_PROB <- predict(object=model_svm, testingData[,predictors])

predictors <- names(validationData)[names(validationData) != labelName]
final_blender_model <- train(validationData[,predictors], validationData[,labelName], method='rf', trControl=myControl)

preds <- predict(object=final_blender_model, testingData[,predictors])
auc <- roc(testingData[,labelName], preds)
print(auc$auc)
