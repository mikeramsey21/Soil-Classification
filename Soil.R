#############################################
# Michael Ramsey
# Date Created: 2/25/18
# Last Updated: 9/4/18
#############################################

# This in an R-script that performs a decision tree classification method to classify 
# soils of 31 types. 

####### Workspace Items #######
# soil_tree: Decision Tree based on all predictors in the dataset
# topsoil_tree: Decision Tree based on only the predictors relevant to topsoil
# topsiol_tree2: Pruned version of topsoil_tree

#####################################################

# Import the data
data <- read.csv('Soils.csv')
str(data)

# Import necessary libraries
library(tree)
library(randomForest) 

#####################################################
# Implement regression tree on the predictors for topsoil only
topsoil_tree <- tree(RSG_Name ~ T_SAND + T_SILT + T_CLAY + T_REF_BULK+ T_BULK_DEN + T_OC +
       T_PH_H2O + T_CEC_CLAY + T_CEC_SOIL + T_BS + T_TEB + T_CACO3 + T_CASO4 + 
       T_ESP + T_ECE, data = data)

# Get summary of the classification tree
summary(topsoil_tree)

# Plot the tree
plot(topsoil_tree)
text(topsoil_tree, all = T)

# Perform cross-validation
cv.tree(topsoil_tree)

# Prune the tree
topsoil_tree2 <- prune.tree(topsoil_tree) # Best 23 nodes
summary(topsoil_tree2)

#####################################################
# Implement regression tree on all predictors
soil_tree <- tree(RSG_Name ~ . - Qualifier_Name - SU_name - FAO_SYS, data = data)
summary(soil_tree)

# Plot the tree
plot(soil_tree)
text(soil_tree, all = T)

# Perform cross-validation
cv.tree(soil_tree)

# Conclusion: Clearly a decision tree method does not have enough variance to adequately 
# describe the data. We move on to Random forests