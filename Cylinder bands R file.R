#FINAL PROJECT - CYLINDER BANDS DATASET

#Setting the directory
setwd("/Users/swathib/Desktop/Final Project")

#Reading the datafile
#install.packages("readxl")
library(readxl)
cylData = read_xlsx("Cylinder Bands dataset.xlsx", sheet = "Sheet2")
head(cylData)
str(cylData)
summary(cylData)


#Converting the incorrectly marked char to numeric variables
cylData[, 18:36] = lapply(cylData[,18:36], as.numeric)
C= cor(cylData[,18:36])
library(corrplot)
library(RColorBrewer)
corrplot(C)
summary(cylData)

#Changing everything to uppercase
cylData[, 1:17] = lapply(cylData[,1:17], toupper)
cylData$bandType = toupper(cylData$bandType)
summary(cylData)

#Converting char to factor
cylData[, 1:17] = lapply(cylData[,1:17], as.factor)
cylData$bandType = as.factor(cylData$bandType)
summary(cylData)

#Deleting constant columns
cylData= cylData[, -c(3,6)]
summary(cylData)

#Histograms for numerical variables
hist(cylData$proofCut)
hist(cylData$viscosity)
hist(cylData$caliper)
hist(cylData$inkTemp)
hist(cylData$humifity)
hist(cylData$roughness)
hist(cylData$bladePressure)
hist(cylData$varnishPct)
hist(cylData$pressSpeed)
hist(cylData$inkPct)
hist(cylData$solventPct)
hist(cylData$ESAVolt)
hist(cylData$ESAAmperage)
hist(cylData$wax)
hist(cylData$hardener)
hist(cylData$rollerDurometer)
hist(cylData$currentDensity)
hist(cylData$anodaSpace)
hist(cylData$chromeContent)


#IMPUTE MISSING VALUES 
#Create a function that finds the mode
findmode<-function(x){
  distinct_values<-unique(x)
  distinct_tabulate<-tabulate(match(x,distinct_values))
  distinct_values[which.max(distinct_tabulate)]
}

#replace missing values by the median for numerical variables and by the mode for categorical variables
library(dplyr)
cylData2<-cylData
for (cols in colnames(cylData2)){
  if (cols %in% names(cylData2[,sapply(cylData2, is.numeric)])) {
    cylData2<-cylData2%>%mutate(!!cols := replace(!!rlang::sym(cols), is.na(!!rlang::sym(cols)), median(!!rlang::sym(cols), na.rm=TRUE)))
  }
  else{
    cylData2<-cylData2%>%
      mutate(!!cols:=replace(!!rlang::sym(cols),!!rlang::sym(cols)=="?"|!!rlang::sym(cols)=="NA",findmode(!!rlang::sym(cols))))
  }
}
cylData2$unitNo = as.factor(cylData2$unitNo)
summary(cylData2)
#Create dummy variables
cylData2<-cylData2%>%
  mutate(dgrainScreened=ifelse(grainScreened=="YES",1,0))%>%
  mutate(dctdInk=ifelse(ctdInk=="YES",1,0))%>%
  mutate(dbladeMfg=ifelse(bladeMfg=="BENTON",1,0))%>%
  mutate(dpaperCoated=ifelse(paperType=="COATED",1,0))%>%
  mutate(dpaperSuper=ifelse(paperType=="SUPER",1,0))%>%
  mutate(dinkCoated=ifelse(inkType=="COATED",1,0))%>%
  mutate(dinkCover=ifelse(inkType=="COVER",1,0))%>%
  mutate(ddirectStream=ifelse(directStream=="YES",1,0))%>%
  mutate(dsolventLine=ifelse(solventType=="LINE",1,0))%>%
  mutate(dsolventNaphta=ifelse(solventType=="NAPHTA",1,0))%>%
  mutate(dcylType=ifelse(cylType=="YES",1,0))%>%
  mutate(dpressAlbert70=ifelse(pressType=="ALBERT70",1,0))%>%
  mutate(dpressMotter70=ifelse(pressType=="MOTTER70",1,0))%>%
  mutate(dpressAMotter94=ifelse(pressType=="MOTTER94",1,0))%>%
  mutate(dpress802=ifelse(press=="802",1,0))%>%
  mutate(dpress813=ifelse(press=="813",1,0))%>%
  mutate(dpress815=ifelse(press=="815",1,0))%>%
  mutate(dpress816=ifelse(press=="816",1,0))%>%
  mutate(dpress821=ifelse(press=="821",1,0))%>%
  mutate(dpress824=ifelse(press=="824",1,0))%>%
  mutate(dpress827=ifelse(press=="827",1,0))%>%
  mutate(dsizeCatalog=ifelse(cylSize=="CATALOG",1,0))%>%
  mutate(dsizeSPIEGEL=ifelse(cylSize=="SPIEGEL",1,0))%>%
  mutate(dcanadian=ifelse(millLoc=="CANADIAN",1,0))%>%
  mutate(dmidEuropean=ifelse(millLoc=="MIDEUROPEAN",1,0))%>%
  mutate(dnorthUs=ifelse(millLoc=="NORTHUS",1,0))%>%
  mutate(dscandinavian=ifelse(millLoc=="SCANDINAVIAN",1,0))%>%
  mutate(dplatIngTank=ifelse(platingTank=="1991",1,0))%>%
  mutate(unit2=ifelse(unitNo=="2",1,0))%>%
  mutate(unit9=ifelse(unitNo=="9",1,0))%>%
  mutate(unit7=ifelse(unitNo=="7",1,0))%>%
  mutate(unit1=ifelse(unitNo=="1",1,0))%>%
  mutate(unit5=ifelse(unitNo=="5",1,0))%>%
  mutate(unit10=ifelse(unitNo=="10",1,0))%>%
  mutate(dbandType=ifelse(bandType=="BAND",1,0))


summary(cylData2)

write.csv(cylData2,"/Users/swathib/Desktop/Cylinder1.csv", row.names = FALSE)

#Regression
cylData3= cylData2[,c(16:34,36:70)]
summary(cylData3)
corrplot(cor(cylData3))
fit = lm(pressSpeed~., data=cylData3)
summary(fit)

#OLS Regression - split into training and testing
set.seed(3457)
trainIndex = sort(sample(nrow(cylData3), nrow(cylData3)*.7))
trainCyl = cylData3[ trainIndex,]
testCyl = cylData3[-trainIndex,]

summary(trainCyl)

#Model 1 - pressSpeed
olsfit = lm(pressSpeed~., data = trainCyl)
summary(fit)

rmseOlsTrain = sqrt(mean(olsfit$residuals^2))
rmseOlsTrain

# Predict on the test set
olsPred = predict(olsfit, testCyl)

#RMSE of the predictions on the test set 
rmseOlsTest = sqrt(mean((olsPred - trainCyl$pressSpeed)^2))
rmseOlsTest

#Correlation table
cor(cylData3) #no variable is too correlated

#Model 2 - inkTemp-REJECTED
olsfit2 = lm(inkTemp~., data = trainCyl)
summary(olsfit2)

rmseOlsTrain2 = sqrt(mean(olsfit2$residuals^2))
rmseOlsTrain2

# Predict on the test set
olsPred2 = predict(olsfit2, testCyl)

#RMSE of the predictions on the test set 
rmseOlsTest2 = sqrt(mean((olsPred2 - trainCyl$inkTemp)^2))
rmseOlsTest2

#Model 3 - inkPct- GOOD RESULTS!
olsfit3 = lm(inkPct~., data = trainCyl)
summary(olsfit3)

rmseOlsTrain3 = sqrt(mean(olsfit3$residuals^2))
rmseOlsTrain3

# Predict on the test set
olsPred3 = predict(olsfit3, testCyl)

#RMSE of the predictions on the test set 
rmseOlsTest3 = sqrt(mean((olsPred3 - trainCyl$inkPct)^2))
rmseOlsTest3

#Model 4 - solventPct-REJECTED
olsfit4 = lm(solventPct~., data = trainCyl)
summary(olsfit4)

rmseOlsTrain4 = sqrt(mean(olsfit4$residuals^2))
rmseOlsTrain4

# Predict on the test set
olsPred4 = predict(olsfit4, testCyl)

#RMSE of the predictions on the test set 
rmseOlsTest4 = sqrt(mean((olsPred4 - trainCyl$solventPct)^2))
rmseOlsTest4

#LOG TRANSFORMAITON
#Regression
fit5 = lm(log(inkTemp) ~ . , data=cylData3)
summary(fit5)


#Trying Ridge for solventPct
library(glmnet)

# Separate the X's (independent variable values)
# and Y's (dependent variable values) as matrices
xTrain = as.matrix(trainCyl[,-which(names(trainCyl)== "solventPct")])   
yTrain = as.matrix(trainCyl$solventPct)  

xTest = as.matrix(testCyl[,-which(names(trainCyl)== "solventPct")])   # Take out "MEDV", column 1
yTest = as.matrix(testCyl$solventPct)    # Take only "MEDV", column 1

#Training
fitRidge = cv.glmnet(xTrain, yTrain, alpha=0, nfolds=7)
fitRidge$lambda.min # The lambda value that minimizes the MSE of the model
fitRidge$lambda.1se # The lambda value of the most regularized model with an error
# within one standard deviation of the minimum error.
plot(fitRidge) # The log-lambda plot for the ridge regression

#Prediction- testing on test set
ridgePred = predict(fitRidge, xTest, s="lambda.1se")
rmseRidge = sqrt(mean((ridgePred - yTest)^2))
rmseRidge   # The RMSE of the test set using Ridge regression.
rmseOlsTrain3

# How much R^2 have we given up?
summary(olsfit)   # Look at the R^2, NOT the adjusted R^2
fitRidge = glmnet(xTrain, yTrain, alpha=0, lambda=0.2638187)
fitRidge  # Remember, %Dev is the R^2.


#LASSO - On solventPct
fitLasso = cv.glmnet(xTrain, yTrain, alpha=1, nfolds=7)
fitLasso$lambda.min  
fitLasso$lambda.1se   

plot(fitLasso)  # Again, notice where the plot dips before rising.
# The top of the graph is the number of variables at that lambda.

# Predict the test set using your Lasso regression model.
lassoPred = predict(fitLasso, xTest, s="lambda.1se")
rmseLasso = sqrt(mean((lassoPred - yTest)^2))
rmseLasso   # Lasso test RMSE
rmseOlsTrain3 # OLS test RMSE
# It's possible the values of the training/testing RMSEs will be higher than Ridge.
# Remember, we're more concerned about the ratio between the RMSEs,
#    rather than their absolute values.

#R2 given up
fitLasso = glmnet(xTrain, yTrain, alpha=1, lambda= 0.2139915)
fitLasso

#Elastic NET on solventPct
fitNet = cv.glmnet(xTrain, yTrain, alpha=0.6, nfolds=7)
fitNet$lambda.min  
fitNet$lambda.1se   

plot(fitNet)  # Again, notice where the plot dips before rising.
# The top of the graph is the number of variables at that lambda.

# Predict the test set using your Lasso regression model.
netPred = predict(fitNet, xTest, s="lambda.1se")
rmseNet = sqrt(mean((netPred - yTest)^2))
rmseNet   # Lasso test RMSE
rmseOlsTrain3 # OLS test RMSE


#R2 given up
fitNet = glmnet(xTrain, yTrain, alpha=0.6, lambda= 0.1694388)
fitNet

#Residual plots
par(mfrow = c(1,1))
plot(olsfit, which= 2)
plot(olsfit2, which= 2)
plot(olsfit3, which=2 )
plot(olsfit4, which=2 )





library(dplyr)
cylData4<-cylData
for (cols in colnames(cylData4)){
  if (cols %in% names(cylData4[,sapply(cylData4, is.numeric)])) {
    cylData4<-cylData4%>%mutate(!!cols := replace(!!rlang::sym(cols), is.na(!!rlang::sym(cols)), median(!!rlang::sym(cols), na.rm=TRUE)))
  }
  else{
    cylData4<-cylData4%>%
      mutate(!!cols:=replace(!!rlang::sym(cols),!!rlang::sym(cols)=="?"|!!rlang::sym(cols)=="NA",findmode(!!rlang::sym(cols))))
  }
}
hist(cylData4$proofCut)
hist(cylData4$viscosity)
hist(cylData4$caliper)
hist(cylData4$inkTemp)
hist(cylData4$humifity)
hist(cylData4$roughness)
hist(cylData4$bladePressure)
hist(cylData4$varnishPct)
hist(cylData4$pressSpeed)
hist(cylData4$inkPct)
hist(cylData4$solventPct)
hist(cylData4$ESAVolt)
hist(cylData4$ESAAmperage)
hist(cylData4$wax)
hist(cylData4$hardener)
hist(cylData4$rollerDurometer)
hist(cylData4$currentDensity)
hist(cylData4$anodaSpace)
hist(cylData4$chromeContent)



#MILESTONE 3
#str(cylData4)
#cylData4=cylData4[,c(-1)]
#x= subset(cylData4, cylData4$bandType=='?')
#cylData4 = droplevels(cylData4,exclude = x)


#Logistic regression


#Dataset
cylData5= cylData2[,c(16:69)]
str(cylData5)
x= subset(cylData5, cylData5$bandType=='?')
cylData5 = droplevels(cylData5,exclude = x)
#Splitting into training and testing
set.seed(47562)
trainIndex1 = sort(sample(nrow(cylData5), nrow(cylData5)*.8))
trainCyl1 = cylData5[ trainIndex,]
testCyl1 = cylData5[-trainIndex,]


model = glm(formula = bandType ~ . ,
            family=binomial(link='logit'), 
            data=trainCyl1)

pred = predict(model, newdata=testCyl1, type='response')
print(pred)   # We get predicted probabilities

# Let's use the .5 cutoff to classify them
pred = ifelse(pred > 0.5, 'NOBAND','BAND')
z=table(pred, testCyl1$bandType) 
z# Look at false positives and false negatives
misClasificError =round(((z[1,2] + z[2,1])/(z[1,1]+z[1,2]+z[2,1]+z[2,2])),digit=2)
print(paste('Accuracy',1-misClasificError))
falseNegRate = z[2,1]/(z[2,1]+z[2,2])
falsePosRate = z[1,2]/(z[1,1]+z[1,2])
# Let's look at the ROC curve
library(ROCR)
p = predict(model, newdata=testCyl1, type="response")
pr = prediction(p, testCyl1$bandType)
prf = performance(pr, measure = "tpr", x.measure = "fpr")
plot(prf)

# Compute the area under the curve
auc = performance(pr, measure = "auc")
auc = auc@y.values[[1]]
auc

#Stepwise selection
library(MASS)
step.model <- model %>% stepAIC(trace = FALSE)
coef(step.model)
step.model
summary(step.model)

#Step model
pred1 = predict(step.model, newdata=testCyl1, type='response')
print(pred1)   # We get predicted probabilities

# Let's use the .5 cutoff to classify them
pred1 = ifelse(pred1 > 0.5, 'NOBAND','BAND')
z1=table(pred1, testCyl1$bandType) 
z1# Look at false positives and false negatives
misClasificError =round(((z1[1,2] + z1[2,1])/(z1[1,1]+z1[1,2]+z1[2,1]+z1[2,2])),digit=2)
print(paste('Accuracy',1-misClasificError))
falseNegRate = z1[2,1]/(z1[2,1]+z1[2,2])
falsePosRate = z1[1,2]/(z1[1,1]+z1[1,2])
# Let's look at the ROC curve
library(ROCR)
p1 = predict(step.model, newdata=testCyl1, type="response")
pr1 = prediction(p1, testCyl1$bandType)
prf1 = performance(pr1, measure = "tpr", x.measure = "fpr")
plot(prf1)

# Compute the area under the curve
auc1 = performance(pr1, measure = "auc")
auc1 = auc1@y.values[[1]]
auc1

#Forward selection
step.model2 <- model %>% stepAIC(trace = FALSE, direction = "forward")
step.model2

summary(step.model2)  

pred2 = predict(step.model2, newdata=testCyl1, type='response')
print(pred2)   # We get predicted probabilities

# Let's use the .5 cutoff to classify them
pred2 = ifelse(pred2 > 0.5, 'NOBAND','BAND')
z2=table(pred2, testCyl1$bandType) 
z2# Look at false positives and false negatives
misClasificError =round(((z2[1,2] + z2[2,1])/(z2[1,1]+z2[1,2]+z2[2,1]+z2[2,2])),digit=2)
print(paste('Accuracy',1-misClasificError))
falseNegRate = z2[2,1]/(z2[2,1]+z2[2,2])
falsePosRate = z2[1,2]/(z2[1,1]+z2[1,2])
# Let's look at the ROC curve
library(ROCR)
p2 = predict(step.model2, newdata=testCyl1, type="response")
pr2 = prediction(p2, testCyl1$bandType)
prf2 = performance(pr2, measure = "tpr", x.measure = "fpr")
plot(prf2)

# Compute the area under the curve
auc2 = performance(pr2, measure = "auc")
auc2 = auc2@y.values[[1]]
auc2

#Backward selection
step.model3 <- model %>% stepAIC(trace = FALSE, direction = "backward")
step.model3

summary(step.model3)  

pred3 = predict(step.model3, newdata=testCyl1, type='response')
print(pred3)   # We get predicted probabilities

# Let's use the .5 cutoff to classify them
pred3 = ifelse(pred3 > 0.5, 'NOBAND','BAND')
z3=table(pred3, testCyl1$bandType) 
z3# Look at false positives and false negatives
misClasificError =round(((z3[1,2] + z3[2,1])/(z3[1,1]+z3[1,2]+z3[2,1]+z3[2,2])),digit=2)
print(paste('Accuracy',1-misClasificError))
falseNegRate = z3[2,1]/(z3[2,1]+z3[2,2])
falsePosRate = z3[1,2]/(z3[1,1]+z3[1,2])
# Let's look at the ROC curve
library(ROCR)
p3 = predict(step.model3, newdata=testCyl1, type="response")
pr3 = prediction(p3, testCyl1$bandType)
prf3 = performance(pr3, measure = "tpr", x.measure = "fpr")
plot(prf3)

# Compute the area under the curve
auc3 = performance(pr3, measure = "auc")
auc3 = auc3@y.values[[1]]
auc3

#Correspondence analysis
#x= subset(cylData2, cylData2$bandType=='?')
#cylData2 = droplevels(cylData2,exclude = x)
#conTab=table(cylData2$paperType,cylData2$bandType)


#library(vcd)
#mosaicplot(conTab, shade=TRUE, xlab="Ink Type", ylab = "Band Type")

#library(ca)
#fit = ca(conTab)
#fit



#Decision tree
#install.packages("tree")
library(tree)
set.seed(47562)
cylData3$dbandType = as.factor(cylData3$dbandType)
trainIndex = sort(sample(nrow(cylData3), nrow(cylData3)*.7))
trainCyl = cylData3[ trainIndex,]
testCyl = cylData3[-trainIndex,]
train.tree.cyl = tree(dbandType ~ ., data = trainCyl)
summary(train.tree.cyl)
plot(train.tree.cyl)
text(train.tree.cyl, pretty=0)

#Predicting on training
tree.cyl = predict(train.tree.cyl, newdata = trainCyl , type="class")
summary(tree.cyl)
plot(tree.cyl)
mistab = with(trainCyl, table(tree.cyl, trainCyl$dbandType))
rownames(mistab) = c("0","1")
colnames(mistab) = c("0","1")
mistab
err = (mistab[1,1] + mistab[2,2])/sum(mistab)
err

#Predicting on test
test.tree.cyl = predict(train.tree.cyl, newdata = testCyl , type="class")
summary(test.tree.cyl)
plot(test.tree.cyl)
mistab = with(testCyl, table(test.tree.cyl, testCyl$dbandType))
mistab
err = (mistab[1,1] + mistab[2,2])/sum(mistab)
err

bandcount = table(cylData3$dbandType)
rownames(bandcount)= c("0","1")
barplot(bandcount, xlab = "Band Type", ylab = "Frequency")
bandcount = table(trainCyl$dbandType)
rownames(bandcount)= c("0","1")
barplot(bandcount, xlab = "Band Type", ylab = "Frequency")
bandcount = table(testCyl$dbandType)
rownames(bandcount)= c("0","1")
barplot(bandcount, xlab = "Band Type", ylab = "Frequency")

#Another dec tree method - doesn't work great
#install.packages("rpart.plot")
library(rpart)
library(rpart.plot)
tree <- rpart(dbandType~., data=trainCyl)
rpart.plot(tree)

testTree = predict(tree, newdata = testCyl , type="class")
summary(testTree)
plot(testTree)
mistab = with(testCyl, table(testTree, testCyl$dbandType))
mistab
err = (mistab[1,1] + mistab[2,2])/sum(mistab)
err



#Regularized
# Separate the X's (independent variable values)
# and Y's (dependent variable values) as matrices

xTrain = as.matrix(trainCyl[,-which(names(trainCyl)== "inkPct")])   

yTrain = as.matrix(trainCyl$inkPct) 
str(xTrain)

xTest = as.matrix(testCyl[,-which(names(trainCyl)== "inkPct")]) 

yTest = as.matrix(testCyl$inkPct)  

#LASSO - On inkPct
fitLasso = cv.glmnet(xTrain, yTrain, alpha=1, nfolds=7)
fitLasso$lambda.min  
fitLasso$lambda.1se   

plot(fitLasso)


coef(fitLasso, c(fitLasso$lambda.min))
# Predict the test set using your Lasso regression model.
lassoPred = predict(fitLasso, xTest, s="lambda.min")
rmseLasso = sqrt(mean((lassoPred - yTest)^2))

rmseLasso   # Lasso test RMSE
rmseOlsTrain3 # OLS test RMSE

#R2 given up
fitLasso = glmnet(xTrain, yTrain, alpha=1, lambda=0.04682913 )
fitLasso

#Elastic NET on inkPct
fitNet = cv.glmnet(xTrain, yTrain, alpha=0.6, nfolds=7)
fitNet$lambda.min  
fitNet$lambda.1se   

plot(fitNet)  

coef(fitNet, c(fitNet$lambda.min))

# Predict the test set using your Lasso regression model.
netPred = predict(fitNet, xTest, s="lambda.min")
rmseNet = sqrt(mean((netPred - yTest)^2))
rmseNet   # Lasso test RMSE
rmseOlsTrain3 # OLS train RMSE


#R2 given up
fitNet = glmnet(xTrain, yTrain, alpha=0.6, lambda= 0.05379584)
fitNet
