###Neural Networks
install.packages("neuralnet")
library(neuralnet)

#Normalize Function
normalize <- function(x) {
  return((x-min(x)) / (max(x)-min(x)))
}

concrete_norm <- as.data.frame(lapply(concrete,normalize))
concrete_train <-concrete_norm[1:773, ]
concrete_test <- concrete_norm[774:1030, ]

#Train a model on data
#train neuralnet model
library(neuralnet)

#simple ANN with only a single hidden neuron
concrete_model <- neuralnet(formula = strength ~ cement + slag +
                            ash + water + superplastic +
                              coarseagg + fineagg +age, 
                            data=concrete_train)
#Visualize network toplology
plot(concrete_model)

#Evaluating the model
#obtain model results
model_results <-compute(concrete_model,concrete_test[1:8])

#obtain predicted strength values
predicted_strength <- model_results$net.result

#examine the corelation between predicted and actual values
cor(predicted_strength,concrete_test$strength)

#Improve Model Performance
concrete_model2 <- neuralnet(formula = strength ~ cement + slag +
                              ash + water + superplastic +
                              coarseagg + fineagg +age, 
                            data=concrete_train,hidden=c(5,2))
plot(concrete_model2)

#####Evaluating the model
model_results2 <-compute(concrete_model2,concrete_test[1:8])
predicted_strength2 <- model_results2$net.result
cor(predicted_strength2,concrete_test$strength)

