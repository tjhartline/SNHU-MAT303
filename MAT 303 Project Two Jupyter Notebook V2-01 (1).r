
print("This step will first install three R packages. Please wait until the packages are fully installed.")
print("Once the installation is complete, this step will print 'Installation complete!'")

install.packages("ResourceSelection")
install.packages("pROC")
install.packages("rpart.plot")

print("Installation complete!")

heart_data <- read.csv(file="heart_disease.csv", header=TRUE, sep=",")

# Converting appropriate variables to factors  
heart_data <- within(heart_data, {
   target <- factor(target)
   sex <- factor(sex)
   cp <- factor(cp)
   fbs <- factor(fbs)
   restecg <- factor(restecg)
   exang <- factor(exang)
   slope <- factor(slope)
   ca <- factor(ca)
   thal <- factor(thal)
})

head(heart_data, 10)

print("Number of variables")
ncol(heart_data)

print("Number of rows")
nrow(heart_data)

# Create 1st logistic model.
logit1 <- glm(target ~ age + trestbps + exang + thalach, data = heart_data, family = "binomial")

summary(logit1)

library(ResourceSelection)

#Perform fit test
htl = hoslem.test(logit1$y, fitted(logit1), g = 50)
htl

#Set default level to 95%
conf_int <- confint.default(logit1, level = 0.95)
round(conf_int,4)

#Create default prediction model
default_model_data <- heart_data[c('age', 'trestbps', 'exang', 'thalach')]
pred <- predict(logit1, newdata = default_model_data, type = 'response')

# If predicted probability having heart disease is >=0.50,
# then predict heart disease (target='1'), Else, do not predict heart-disease. 
# (target='0') 
var_pred = as.factor(ifelse(pred >= 0.5, '1', '0'))

library("pROC")
labs <- heart_data$target
preds <- logit1$fitted.values

roc1 <- roc(labs ~ preds)

#Output AUC
cat("Area Under the Curve-AUC")
round(auc(roc1), 4)

#Output ROC 
cat("ROC Curve")

#plot TPR(Sensitivity), FPR(1-Specificity) Set to true
plot(roc1, legacy.axes = 1)

#Create default prediction model
default_model_data <- heart_data[c('age', 'trestbps', 'exang', 'thalach')]
pred <- predict(logit1, newdata = default_model_data, type = 'response')

# If predicted probability having heart disease is >=0.50,
# then predict heart disease (target='1'), Else, do not predict heart-disease. 
# (target='0') 
var_pred = as.factor(ifelse(pred >= 0.5, '1', '0'))

library("pROC")

labels <- heart_data$target
preds <- logit1$fitted.values

roc <- roc(labels ~ preds)

cat("Area Under the Curve - (AUC)")
round(auc(roc),4)

cat("--ROC Curve--\n")

plot(roc, legacy.axes = 1)

cat("Prediction for Heart Disease Where Age is 50, Trestbps is 122, Exang is True, and Thalach is 140")
newdata1 <- data.frame(age = 50, trestbps = 122, exang = '1', thalach = 140)
pred1 <- predict(logit1, newdata1, type = 'response')
round(pred1, 4)

cat("Prediction for Heart Disease Where Age is 50, Trestbps is 130, Exang is False, and Thalach is 165")
newdata2 <- data.frame(age = 50, trestbps = 130, exang = '0', thalach = 165 )
pred2 <- predict(logit1, newdata2, type = 'response')
round(pred2, 4)

# Create 2nd regression model
logit2 <- glm(target ~ age + trestbps + cp + thalach + I(age^2) + age:thalach, data = heart_data, family = "binomial")

summary(logit2)

library(ResourceSelection)

ht2 = hoslem.test(logit2$y, fitted(logit2), g=50)
ht2

# Create default prediction model
default_model_data <- heart_data[c('age', 'trestbps', 'cp', 'thalach')]
pred <- predict(logit2, newdata = default_model_data, type = 'response')

# If predicted probability of having heart disease is >=0.50,
# then predict target (target='1'), Else, do not predict heart disease
# (target='0') 
var_pred = as.factor(ifelse(pred >= 0.5, '1', '0'))

# Create confusion matrix
conf.matrix <- table(heart_data$target, var_pred)[c('0','1'), c('0','1')]
rownames(conf.matrix) <- paste("Actual", rownames(conf.matrix), sep = ": target=")
colnames(conf.matrix) <- paste("Prediction", colnames(conf.matrix), sep = ": target=")

# Format confusion matrix
cat("Confusion Matrix")
format(conf.matrix, justify = "centre", digit = 2)

library("pROC")

labels <- heart_data$target
preds <- logit2$fitted.values

roc <- roc(labels ~ preds)

cat("Area Under the Curve - (AUC)")
round(auc(roc),4)

cat("--ROC Curve--\n")
plot(roc, legacy.axes = 1)

cat("Prediction for Heart Disease where age is 50, Trestbps is 115, CP is False, and Thalach is 133")
newdata1 <- data.frame(age = 50, trestbps = 115, cp = "0", thalach = 133)
pred1 <- predict(logit2, newdata1, type='response')
round(pred1, 4)

cat("Prediction for Heart Disease where age is 50, Trestbps is 125, CP is True, and Thalach is 155")
newdata2 <- data.frame(age = 50, trestbps = 125, cp = "1", thalach = 155)
pred2 <- predict(logit2, newdata2, type='response')
round(pred2, 4)

set.seed(6522048)

# Split into sets (Training/Testing-Validation)
samp.size = floor(0.85*nrow(heart_data))

# Training set
cat("Training Set Row Count")
train_ind = sample(seq_len(nrow(heart_data)), size = samp.size)
train.data = heart_data[train_ind,]
nrow(train.data)

# Testing set 
cat("Validation Set Row Count")
test.data = heart_data[-train_ind,]
nrow(test.data)

set.seed(6522048)
library(randomForest)

# checking
#=====================================================================
train = c()
test = c()
trees = c()

for(i in seq(from=1, to=150, by=1)) {
    #print(i)
    
    trees <- c(trees, i)
    
    model_rf1 <- randomForest(target ~ age + sex + cp + trestbps + chol + restecg + exang + ca, data = train.data, ntree = i)
    
    train.data.predict <- predict(model_rf1, train.data, type = "class")
    conf.matrix1 <- table(train.data$target, train.data.predict)
    train_error = 1-(sum(diag(conf.matrix1)))/sum(conf.matrix1)
    train <- c(train, train_error)
    
    test.data.predict <- predict(model_rf1, test.data, type = "class")
    conf.matrix2 <- table(test.data$target, test.data.predict)
    test_error = 1-(sum(diag(conf.matrix2)))/sum(conf.matrix2)
    test <- c(test, test_error)
}


#matplot (trees, cbind (train, test), ylim=c(0,0.5) , type = c("l", "l"), lwd=2, col=c("red","blue"), ylab="Error", xlab="number of trees")
#legend('topright',legend = c('training set','testing set'), col = c("red","blue"), lwd = 2 )

plot(trees, 
     train, 
     type = "l", 
     ylim=c(0,1.0),
     col = "orange", 
     xlab = "Number of Trees", 
     ylab = "Classification Error")

lines(test, 
      type = "l", 
      col = "purple")

legend('topright',
       legend = c('training set','testing set'), 
       col = c("orange","purple"), 
       lwd = 2 )


set.seed(6522048)
library(randomForest)
model_rf4 <- randomForest(target ~ age + sex + cp + trestbps + chol + restecg + exang + slope + ca, data = train.data, ntree = 25)

# Confusion matrix
cat("===============================================================================\n")
cat('Confusion Matrix:TRAINING set based on random forest model built using 25 trees')
train.data.predict <- predict(model_rf4, train.data, type = "class")

# Construct the confusion matrix
conf.matrix <- table(train.data$target, train.data.predict)[,c('0','1')]
rownames(conf.matrix) <- paste("Actual", rownames(conf.matrix), sep = ": ")
colnames(conf.matrix) <- paste("Prediction", colnames(conf.matrix), sep = ": ")

# Print nicely formatted confusion matrix
format(conf.matrix, justify = "centre", digit = 2)


cat("===============================================================================\n")
cat('Confusion Matrix: TESTING set based on random forest model built using 25 trees')
test.data.predict <- predict(model_rf4, test.data, type = "class")

# Construct the confusion matrix
conf.matrix <- table(test.data$target, test.data.predict)[,c('0','1')]
rownames(conf.matrix) <- paste("Actual", rownames(conf.matrix), sep = ": ")
colnames(conf.matrix) <- paste("Prediction", colnames(conf.matrix), sep = ": ")

# Print nicely formatted confusion matrix
format(conf.matrix, justify = "centre", digit = 2)

set.seed(6522048)

# Split into training and validation sets
samp.size = floor(0.80*nrow(heart_data))

# Training set
cat("Row Count in Training Set")
train_ind = sample(seq_len(nrow(heart_data)), size = samp.size)
train.data = heart_data[train_ind,]
nrow(train.data)

# Testing set 
cat("Row Count in Validation Set")
test.data = heart_data[-train_ind,]
nrow(test.data)

set.seed(6522048)
library(randomForest)

# Root mean squared error
RMSE = function(pred, obs) {
    return(sqrt( sum( (pred - obs)^2 )/length(pred) ) )
}

# checking
train = c()
test = c()
trees = c()

for(i in seq(from = 1, to = 80, by = 1)) {
    trees <- c(trees, i)
    model_rf7 <- randomForest(thalach ~ age + sex + cp + trestbps + chol + restecg + exang + ca, data = train.data, ntree = i)
    
    pred <- predict(model_rf7, newdata=train.data, type='response')
    rmse_train <-  RMSE(pred, train.data$thalach)
    train <- c(train, rmse_train)
    
    pred <- predict(model_rf7, newdata=test.data, type='response')
    rmse_test <-  RMSE(pred, test.data$thalach)
    test <- c(test, rmse_test)
}

plot(trees, 
     train,type = "l",
     ylim = c(1,100),
     col = "green", 
     xlab = "Number of Trees", 
     ylab = "Root Mean Squared Error")
lines(test, 
      type = "l", 
      col = "red")
legend('topright',
       legend = c('training set','testing set'), 
       col = c("green","red"), 
       lwd = 2 )


set.seed(6522048)
library(randomForest)
model_rf8 <- randomForest(thalach ~ age + sex + cp + trestbps + chol + restecg + exang + ca, data = train.data, ntree = 25)


# Root mean squared error
RMSE = function(pred, obs) {
    return(sqrt( sum( (pred - obs)^2 )/length(pred) ) )
}

cat("=============================================================================\n")
cat('Root Mean Squared Error: TRAINING set based on RF model built using 25 trees')
pred <- predict(model_rf8, newdata=train.data, type='response')
formatRMSE = RMSE(pred, train.data$thalach)
round(formatRMSE, 4)


cat("============================================================================\n")
cat('Root Mean Squared Error: TESTING set based on RF model built using 25 trees')
pred <- predict(model_rf8, newdata=test.data, type='response')
formatRMSE = RMSE(pred, test.data$thalach)
round(formatRMSE, 4)
