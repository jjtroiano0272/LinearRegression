#################################################################################################
################################### AS A GENERALIZED THING    ###################################
################################### CODE THIS IN AS A GENERAL ###################################
################################### FUNCTION THAT CAN RUN ON  ###################################
################################### AN OBJECT.                ###################################
#################################################################################################

# Create vectors X and Y to g into the Data frame
X <- c(1.4, 1.8, 2.6, 3, 3.4, 4.3, 5.00, 6.00)
Y <- c(2.2, 3.8, 5.6, 6, 6.6, 8.5, 10.1, 11.9)

# err_term <- 0 # Error term is not multiplicative so it won't affect output if it's 0.
raw_data <- data.frame(X,Y)
model <- lm(X ~ Y, data = raw_data) #Create linear model between X and Y

# Intercept  + Y(actualValue) * Y
# The p-value is way below .05 so what we're looking at must be quite statistically significant.
# When this is run, the intercept is y and the value 'Y' on the right is the actual beta coefficient.
# print(model)
# summary(model)

# STEP 2
# WARNING: NOT MY O.C.!
#Create Training and Test data -
set.seed(100)  # setting seed to reproduce results of random sampling
trainingRowIndex <- sample(1:nrow(raw_data), 0.8*nrow(raw_data))  # row indices for training data
trainingData <- raw_data[trainingRowIndex, ]  # model training data
# test data; JT: these should be the rows from the original data, absent from the training model. This is validation.
testData  <- raw_data[-trainingRowIndex, ]

# STEP 3
# WARNING: NOT MY O.C.!
# Build the model on training data -
lmMod <- lm(X ~ Y, data = trainingData)  # build the model
YPredictor <- predict(lmMod, testData)  # predict distance

# STEP 4
# WARNING: NOT MY O.C.!
# [OC] Creates frame comparing the actual values to those predicted by our model.
predicted_actuals <- data.frame(cbind(actuals = testData$Y, predicteds = YPredictor))
# Compares (runs correlation) on the two inputs to predicted/actual data frame.
pred_actual_correlation <- cor(predicted_actuals)
# 0.49 in our case
min_max_accuracy <- mean( apply(predicted_actuals, 1, min) / apply(predicted_actuals, 1, max) )
abs_perc_err <- mean( abs((predicted_actuals$predicteds - predicted_actuals$actuals))/predicted_actuals$actuals )


# Iteratively estimate the parameters using for.
# This continues until the error at step k, e_k < 0.0005 or it's iterated more than 2,000 times.
for (i in 1:2000) {
  # Use this control flow portion to exit if the error is less than 0.0005
  if (err_term < 0.0005) {
    break # Check that this exits the whole loop
  }
  
  err_term <- err_term + 0.0100
  
  print("foo")
}

#What is B_k+1 programmatically?