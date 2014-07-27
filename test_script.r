rm(list = ls())  # clear workspace

# All packages are assumed to be installed
library(ElemStatLearn)
library(caret)
library(gbm)
pml_training_set <- read.csv('pml-training.csv')
pml_testing <- read.csv('pml-testing.csv')

  # The first 7 variables are not useful for prediction
pml_training_set <- pml_training_set[8:160]

# columns with more than ten-percent NAs for values are useless for prediction
tmn <- names(colSums(is.na(pml_training_set))[  colSums(is.na(pml_training_set))  > (nrow (pml_training_set) * 0.1) ])
k <- setdiff (names(pml_training_set), tmn)
pml_training_set <- pml_training_set[, k]

  # Columns were little variance are not helpful for prediction
pml_training_set <- pml_training_set[, -nearZeroVar(pml_training_set)]

  # partition the data 60% training, 40% validation
dataPartition <- createDataPartition (pml_training_set$classe, p = 0.60, list = FALSE)[, 1]
training <- pml_training_set[dataPartition, ]
validate <- pml_training_set[-dataPartition, ]

  # Train a model
zeTrain <- trainControl(method='cv',number=10,repeats=1)
modelFit_gbm <- train(classe ~ ., data = training, method = "gbm", trControl = zeTrain)

# Attempts were made with these methods, but strange errors resulted so its assumed they
# were impractical or nonapplicable
# > modelFit_ada <- train(classe ~ ., data = training, method = "ada", trControl = zeTrain)
# > modelFit_rf <- train(classe ~ ., data = training, method = "rf", trControl = zeTrain)


validate["predicted_gbm"] <- predict(modelFit_gbm, newdata=validate)
# A confusion matrix is a convenient way to look for accuracy
# In this case, tha accuracy is 96.11%
confusionMatrix(validate$classe,validate$predicted_gbm)
varImp(modelFit_gbm)  # Helps list the importance of the various variales

# Now we predict using out model on the test set
pml_testing["predicted_gbm"] <- predict(modelFit_gbm, newdata=pml_testing)
answers <- pml_testing$predicted_gbm


# Outputting the solutions here
dir.create ('solutions')
n = length(answers)
for(i in 1:n)
{
  filename = paste("problem_id_",i,".txt")
  filename = file.path('solutions', filename)
  write.table (answers[i], file=filename, quote=FALSE, row.names=FALSE, col.names=FALSE)
}





