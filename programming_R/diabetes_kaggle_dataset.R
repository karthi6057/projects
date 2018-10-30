dataset<-read.csv("C:/Users/Mohan/Desktop/diabetes.csv")
View(dataset)
dataset$Outcome<-as.factor(dataset$Outcome)

library(Amelia)
missmap(dataset,col=c("yellow","black"))

library(corrplot)
M<-cor(dataset[-9])
corrplot(M,method="number",tl.cex=0.7)

#Agegroup
min(dataset$Age)
Agegroup=cut(dataset$Age,breaks=c(20,35,50,100),labels=c("A","B","C"))

library(ggplot2)

ggplot(data=dataset, aes(Glucose,BloodPressure)) + 
  geom_jitter(aes(colour = Outcome))+
  labs(title="Glucose vs BloodPressure")+
  theme_light()

graph=function(a,b){
  ggplot(data=dataset,aes(Agegroup,a))+
    geom_boxplot(aes(colour=Outcome))+
    labs(x="Agegroup",y=b)
}

g1=graph(dataset$BloodPressure,"Bloodpressure")
g2=graph(dataset$Pregnancies,"Pregnancies")
g3=graph(dataset$Glucose,"Glucose")
g4=graph(dataset$Insulin,"Insulin")
g5=graph(dataset$BMI,"BMI")
g6=graph(dataset$DiabetesPedigreeFunction,"DiabetesPedigreeFunction")
g7=graph(dataset$SkinThickness,"SkinThickness")

library(gridExtra)
grid.arrange(g1,g2,g3,g4,g5,g6,g7,ncol=3)

#hypothesis test
t.test(dataset$Pregnancies~dataset$Outcome)#-6.8

t.test(dataset$Glucose~dataset$Outcome)#-2.2

t.test(dataset$BloodPressure~dataset$Outcome)#0.08

t.test(dataset$SkinThickness~dataset$Outcome)#0.04

t.test(dataset$Insulin~dataset$Outcome)#0.001

t.test(dataset$BMI~dataset$Outcome)#-2.2

t.test(dataset$DiabetesPedigreeFunction~dataset$Outcome)#-6.1

t.test(dataset$Age~dataset$Outcome)#-1.2

#Splitting the data
set.seed(439)
indx<-sample(1:nrow(dataset),600)
train<-dataset[indx,]
test<-dataset[-indx,]

#Model
model<-glm(Outcome~.,family=binomial(),data=train)
summary(model)

ypred<-predict(model,type="response",newdata=test[-9])
ypred<-ifelse(ypred>0.5,1,0)

#confusion matrix
table(test$Outcome,ypred)

#Model
model2<-glm(Outcome~Pregnancies+Glucose+BloodPressure+Insulin+BMI+DiabetesPedigreeFunction+Age ,family=binomial(),data=train)
summary(model2)

ypred2<-predict(model2,type="response",newdata=test[-9])
ypred2<-ifelse(ypred2>0.5,1,0)

#confusion matrix
table(test$Outcome,ypred2)
#Accuracy
Acc<-139/168
Acc

#K-Fold cross validation
library(caret)
cross<-trainControl(method="cv",number=10)
cv<-train(Outcome~.,method="glm",data=train,trControl=cross)
cv

#ROC
library(ROCR)
Roc<-prediction(ypred,test$Outcome)
roc.perf<-performance(Roc,measure="tpr",x.measure = "fpr")
plot(roc.perf,colorsize=T)

auc<-performance(Roc,measure="auc")
auc@y.values[[1]]

#RandomForset
library(randomForest)
set.seed(111)
model3<-randomForest(Outcome~Pregnancies+Glucose+BloodPressure+Insulin+BMI+DiabetesPedigreeFunction+Age,data=train,ntree=100)
summary(model3)

importance(model3)

ypred3<-predict(model3,newdata=test[-9])

#confusion matrix
table(test$Outcome,ypred3)
#Accuracy
Acc<-130/168
Acc

#SVM Model
library(e1071)
model4<-svm(Outcome~.,data=train,type="C-classification",kernal="Radial",C=0.5)
summary(model4)

ypred4<-predict(model4,newdata=test[-9])

#confusion matrix
table(test$Outcome,ypred4)
#Accuracy
Acc<-135/168
Acc

#K-Fold cross validation
library(caret)
crss<-trainControl(method="cv",number=10)
cv1<-train(Outcome~.,method="svmRadial",data=train,trControl=crss)
cv1

#Hyperparameter Tuning
libaray(caret)
classifier<-train(Outcome~.,data=train,method="svmRadial")
classifier$bestTune

#Model
model5<-glm(Outcome~Pregnancies+Glucose+BloodPressure+Insulin+BMI ,family=binomial(),data=train)
summary(model5)

ypred5<-predict(model5,type="response",newdata=test[-9])
ypred5<-ifelse(ypred5>0.5,1,0)

#confusion matrix
table(test$Outcome,ypred5)

#Accuracy
Acc<-137/168
Acc

#K-Fold cross validation
library(caret)
crss<-trainControl(method="cv",number=10)
cv2<-train(Outcome~Pregnancies+Glucose+BloodPressure+Insulin+BMI,method="glm",data=train,trControl=crss)
cv2
