install.packages("mlbench")
# Importing libraries
library(mlbench) # Contains several benchmark data sets (especially the Boston Housing dataset)
library(caret) # Package for machine learning algorithms / CARET stands for Classification And REgression Training

# Importing the Boston Housing data set
data(BostonHousing)
data_Bhouse <- BostonHousing
head(data_Bhouse)
View(data_Bhouse)
# Check to see if there are missing data?
sum(is.na(data_Bhouse))

# To achieve reproducible model; set the random seed number
set.seed(100)

# Performs stratified random split of the data set
TrainingIndex <- createDataPartition(data_Bhouse$medv, p=0.8, list = FALSE)
TrainingSet <- data_Bhouse[TrainingIndex,] # Training Set
TestingSet <- data_Bhouse[-TrainingIndex,] # Test Set

###############################

# Build Training model
Model <- train(medv ~ ., data = TrainingSet,
               method = "lm",
               na.action = na.omit,
               preProcess=c("scale","center"),
               trControl= trainControl(method="none")
)

# Apply model for prediction

Model.training <-predict(Model, TrainingSet) # Apply model to make prediction on Training set
Model.testing <-predict(Model, TestingSet) # Apply model to make prediction on Testing set


# Model performance (Displays scatter plot and performance metrics)

# Scatter plot of Training set

plot(TrainingSet$medv,Model.training, col = "blue" )
plot(TestingSet$medv,Model.testing, col = "green" )


# Feature importance
Importance <- varImp(Model)
plot(Importance)
plot(Importance, col = "red")
