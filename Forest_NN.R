install.packages("neuralnet")
library(neuralnet)


View(forestfires)

#Normalize Function
normalize<-function(x){
  return((x-min(x))/(max(x)-min(x)))
}


forestfires_norm<-as.data.frame(lapply(forestfires[3:30],normalize))
train<-forestfires_norm[1:470,]
test<-forestfires_norm[471:517,]

library(neuralnet)
#simple ANN with only a single hidden neuron
forest_model<-neuralnet(formula= area~.,data=train)

#Visualize network toplology
plot(forest_model)

#Evaluating the model
#obtaining model results
model_results<-compute(forest_model,test)

#Obtaining Predicted Area
predicted_area<-model_results$net.result

#Examine Correlation
cor(predicted_area,test$area)

#Improve Model Performance
forest_model_2<- neuralnet(formula = area~., data =train, hidden = c(5,2))
plot(forest_model_2)


#Evaluating Model Performance
model_results_2<-compute(forest_model_2,test)
predicted_model_2<- model_results_2$net.result
cor(predicted_model_2,test$area)

#Improve Model Performance
forest_model_3<- neuralnet(formula = area~., data =train, hidden = c(5,2,1))
plot(forest_model_3)


#Evaluating Model Performance
model_results_3<-compute(forest_model_3,test)
predicted_model_3<- model_results_3$net.result
cor(predicted_model_3,test$area)


