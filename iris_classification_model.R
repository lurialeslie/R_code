# Importing libraries
library(datasets) # Containening the Iris data set
library(caret) # Package for machine learning algorithms / CARET stands for Classification And REgression Training
install.packages("e1071")
library("e1071")
# Importing the Iris data set
data(iris)

iris_trial<- iris
# Check to see if there are missing data?
sum(is.na(iris))

# To achieve reproducible model; set the random seed number
set.seed(100)

# Performs stratified random split of the data set
TrainingIndex <- createDataPartition(iris_trial$Species, p=0.8, list = FALSE)
TrainingSet <- iris_trial[TrainingIndex,] # Training Set
TestingSet <- iris_trial[-TrainingIndex,] # Test Set

# Compare scatter plot of the 80 and 20 data subsets
plot(TrainingSet$Sepal.Width, TrainingSet$Sepal.Length, col="blue")
plot(TestingSet$Sepal.Width, TestingSet$Sepal.Length, col="red")


###############################
# SVM model (polynomial kernel)

# Build Training model
Model <- train(Species ~ ., data = TrainingSet,
               method = "svmPoly",
               na.action = na.omit,
               preProcess=c("scale","center"),
               trControl= trainControl(method="none"),
               tuneGrid = data.frame(degree=1,scale=1,C=1)
)

# Build CV model
Model.cv <- train(Species ~ ., data = TrainingSet,
                  method = "svmPoly",
                  na.action = na.omit,
                  preProcess=c("scale","center"),
                  trControl= trainControl(method="cv", number=10),
                  tuneGrid = data.frame(degree=1,scale=1,C=1)
)


# Apply model for prediction
Model.training <-predict(Model, TrainingSet) # Apply model to make prediction on Training set
Model.testing <-predict(Model, TestingSet) # Apply model to make prediction on Testing set
Model.cv <-predict(Model.cv, TrainingSet) # Perform cross-validation


# Model performance (Displays confusion matrix and statistics)
Model.training.confusion <-confusionMatrix(Model.training, TrainingSet$Species)
Model.testing.confusion <-confusionMatrix(Model.testing, TestingSet$Species)
Model.cv.confusion <-confusionMatrix(Model.cv, TrainingSet$Species)

#print model
print(Model.training.confusion)
print(Model.testing.confusion)
print(Model.cv.confusion)

# Feature importance
Importance <- varImp(Model)
plot(Importance)
plot(Importance, col = "red")
