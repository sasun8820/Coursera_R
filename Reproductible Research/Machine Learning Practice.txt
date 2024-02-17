
#install.packages("kernlab")
library(kernlab)
data(spam)

set.seed(3435)


#### Exploratory Data Analysis ####

# Set a training set 
trainIndicator = rbinom(4681, size =1, prob=0.5)
table(trainIndicator)

# Apply the training set to SPAM dataset 
trainSpam = spam[trainIndicator == 1, ]
testSpam = spam[trainIndicator == 0, ]


# train Spam Type 
table(trainSpam$type)
table(testSpam$type)


# Make a plot 
plot(log10(trainSpam$capitalAve) ~ trainSpam$type)

plot(log10(trainSpam[, 1:4] + 1)) # correlation plots between variables 

# Clustering 
hCluster = hclust(dist(t(log10(trainSpam[, 1:57] + 1 ))))
plot(hCluster)
help(hclust)


#### Statistical Modeling ####

# Modeling to logistic / quadratic/ etc.
trainSpam$numType = as.numeric(trainSpam$type) - 1
costFunction = function(x,y) sum(x != (y>0.5))
cvError = rep(NA, 55)
library(boot)

for (i in 1:55) {
     lmformula = reformulate(names(trainSpam)[i], response = 'numType')
     glmfit = glm(lmformula, family='binomial', data=trainSpam)
     cvError[i] = cv.glm(trainSpam, glmfit, costFunction, 2)$delta[2]
}

# which predictor has minimum corss-validated error?
names(trainSpam)[which.min(cvError)]

# Get a measure of uncertainty (using the best variable and response variable)
predictionModel = glm(numType ~ charDollar, family= 'binomial', data = trainSpam)

# Get prediction on the test set
# ---- Predictionmodel를 사용해서 각 확률을 구하는 것 ----
predictionTest = predict(predictionModel, testSpam)  
predictedSpam = rep("nonspam", dim(testSpam)[1])

# Classify as 'spam' for those with prob > 0.5
predictedSpam[predictionModel$fitted > 0.5] = 'spam'

#install.packages("forecast")
library(forecast)

# Classification of a table --> predicted model values vs. actual test value 
table(predictedSpam, testSpam$type)

# Error Rate
(61+458) / (61+ 458 + 1346 + 449)



#### Interpret Results ####

#### Challenge Results ####


