#############################################
# Michael Ramsey
# Date Created: 2/25/18
# Last Updated: 9/4/18
#############################################

# This in an R-script that performs the Random Forest classification method to classify 
# soils of 31 types. 

# Code adapted from https://www.r-bloggers.com/random-forests-in-r/

####### Workspace Items #######
# topsoil_forest2: Random Forest method for predicting class with topsoil preditores only
# conf2: Confusion matrix associated with classification with model topsoil_forest2
# acc2: Accuracy of classification for topsoil_forest2: (OOB accuracy I believe)
# crossv: Cross Validation to determine optimal number of predictors for each tree

# VALUES
# oob.err: Out-of-Bag eroor
# test.err: Testing error

#####################################################
# Import the data
data <- read.csv('Soils.csv')
str(data)

# Import necessary libraries
library(tree)
library(randomForest) 

#####################################################
# Implement random forest

# Compute the random forest for the topsoil data
topsoil_forest2 <- randomForest(RSG_Name ~ T_SAND + T_SILT + T_CLAY + T_REF_BULK+ T_BULK_DEN + T_OC +
                                 T_PH_H2O + T_CEC_CLAY + T_CEC_SOIL + T_BS + T_TEB + T_CACO3 + T_CASO4 + 
                                 T_ESP + T_ECE, data = data, mtry = 2, ntree = 100)
summary(topsoil_forest2)

# Yields a confusion matrix and info about tree
print('Confustion Matrix for the clasificaiton')
conf2 <- print(unname(topsoil_forest2$confusion))

# Compute accuracy
acc2 <- sum(diag(conf2))/sum(conf2)

# Plot the classification error of the tree
# Dark line shows the overall error vs. number of trees
# Colored lines represent errors of specific classes
plot(topsoil_forest2, type = 'l', main = 'OOB Classification Error')
# One soil type is classified really poorly - due to lack of data
# Typical of random forests

# This just plots the oob error rate as a function of number of trees
# for the overall model
plot(c(1:100), topsoil_forest2$err.rate[,1], type = 'l',
     main = 'OOB Classification Error')

# Find and plot the most important predictors
varImpPlot(topsoil_forest2, main = 'Variable Importance Plot')

# Create partial dependence plots for classification of Ferralsols
# (Default first class)
impvar <- rownames(imp)[order(imp[, 1], decreasing=TRUE)]
for (i in seq_along(impvar)) {
  partialPlot(topsoil_forest2, data, impvar[i], xlab=impvar[i],
              main=paste("Partial Dependence on", impvar[i]))
}

# Perform Cross-Validation
crossv <- rfcv(data[,3:17], data$RSG_Name, cv.fold=10, step=2, recursive=FALSE, ntree = 100)
# It seems 4-8 predictors is best

######################################################
# Calculate the OOB error rate as a function of number of variables
# randomly samples as candidates at each split. We go up to 8 candidates
# We also invesigate a test error rate that none of the trees have seen

# Seperate into training set
train <- sample(1:nrow(data),45449-9000)

# Initialize vectors to store OOB and Test Error
oob.err=double(13)
test.err=double(13)

#Loop through values for mtry
for(mtry in 1:8) 
{
  
  # Train the random forest
  rf <- randomForest(RSG_Name ~ T_SAND + T_SILT + T_CLAY + T_REF_BULK+ T_BULK_DEN + T_OC +
            T_PH_H2O + T_CEC_CLAY + T_CEC_SOIL + T_BS + T_TEB + T_CACO3 + T_CASO4 + 
            T_ESP + T_ECE, data = data, subset = train, mtry=mtry, ntree = 100)
  
  # Record OOB error
  oob.err[mtry] = rf$err.rate[100,1]
  
  # Predict classification for training set
  pred <- predict(rf,data[-train,])
  
  # Get actual classification for training set
  actual <- with(data[-train,], RSG_Name)
  
  # Count number of correct classifications
  numcor <- 0
  for (j in c(1:length(pred))) {
    if (pred[j] == actual[j]) {
      numcor <- numcor + 1
    }
  }
  
  #Compute the error of classification
  test.err[mtry] <- numcor/length(pred)    
  cat(mtry," ") #printing the output to the console
}

# Plot the accuracies above
matplot(1:8, cbind(1-oob.err[1:8],test.err[1:8]), pch=19 , col=c("red","blue"),type="b",ylab="Accuracy",xlab="Number of Predictors Considered at each Split")
legend("topright",legend=c("OOB Accuracy","Test Accuracy"),col=c("red","blue"),pch=19)

# Conclusion: Use 2-5 predictors for classification