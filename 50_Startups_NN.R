install.packages("neuralnet")
library(neuralnet)

Startups<- `50_Startups`[,-4]
View(Startups)

#Normalize Function
normalize<- function(x) {
  return((x-min(x))/(max(x)-min(x)))
}

Startups_norm<-as.data.frame(lapply(Startups,normalize))

#Divide Data in Training and Testing
startups_train<-Startups_norm[1:30,]
startups_test<-Startups_norm[31:50,]


#simple ANN with only a single hidden neuron
startups_model<-neuralnet(formula= Profit~R.D.Spend+Administration+Marketing.Spend,data=startups_train)

#Visualize network toplology
plot(startups_model)

#Evaluating the model
#obtain model results
model_results<-compute(startups_model,startups_test[1:3])

#obtain predicted strength values
predicted_profit<-model_results$net.result

#examine the corelation between predicted and actual values
cor(predicted_profit,startups_test$Profit)


#Improve Model Performance
startups_model2<-neuralnet(formula = Profit~R.D.Spend+Administration+Marketing.Spend,data=startups_train,hidden = c(3,3,2))
plot(startups_model2)

#####Evaluating the model
model_results2<-compute(startups_model2,startups_test[1:3])
predicted_profit2<-model_results2$net.result
cor(predicted_profit2,startups_test$Profit)

#Improve Model Performance
startups_model3<-neuralnet(formula= Profit~.,data=startups_train,hidden= c(4,3,2))
plot(startups_model3)

model_results3<-compute(startups_model3,startups_test[1:3])
predicted_profit3<- model_results3$net.result
cor(predicted_profit3,startups_test$Profit)
