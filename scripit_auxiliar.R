library(caret)
library(mlbench)
emails <- read.csv("Data/emails.csv")
set.seed(107)

#serve como o divisor de grupos, 
inTrain <- createDataPartition(y = emails$spam,
                               ## os dados usados para criar a partição
                               p = .75,
                               ## a porcentagem dos dados para treino
                               list = FALSE)
#set de treino
treino <- emails[inTrain,]

#set de teste
teste <- emails[-inTrain,]


#Usando o KNN
control = trainControl(method = "repeatedcv", number=10,repeats = 10)
knn_grid = expand.grid(k=c(1:1))
knn_model = train(spam ~ .,
                  data=treino ,
                  method="knn",
                  preProcess=c("range"),
                  tuneGrid = knn_grid,
                  trControl=control, 
                  na.action=na.omit)
#Usando o SVM
control <- trainControl(method="repeatedcv", number=10,repeats=10)
modelSvm <- train(spam ~ ., 
                  data=treino, 
                  method="svmRadial", 
                  trControl=control)

#Usando C50
control <- trainControl(method="repeatedcv", number=10, repeats=10)
modelC50<- train(spam ~ .,
                 data=treino, 
                 method="C5.0", 
                 trControl=control)

####Usando 50% treino
#serve como o divisor de grupos, 
inTrain <- createDataPartition(y = emails$spam,
                               ## os dados usados para criar a partição
                               p = .50,
                               ## a porcentagem dos dados para treino
                               list = FALSE)
#set de treino
treino <- emails[inTrain,]

#set de teste
teste <- emails[-inTrain,]


#Usando o KNN
control = trainControl(method = "repeatedcv", number=10,repeats = 10)
knn_grid = expand.grid(k=c(1:20))
knn_model = train(spam ~ .,
                  data=treino ,
                  method="knn",
                  preProcess=c("range"),
                  tuneGrid = knn_grid,
                  trControl=control, 
                  na.action=na.omit)
#Usando o SVM
control <- trainControl(method="repeatedcv", number=10,repeats=10)
modelSvm <- train(spam ~ ., 
                  data=treino, 
                  method="svmRadial", 
                  trControl=control)

#Usando C50
control <- trainControl(method="repeatedcv", number=10, repeats=10)
modelC50<- train(spam ~ .,
                 data=treino, 
                 method="C5.0", 
                 trControl=control)
treino_Fac <-  treino
treino_fac[,"sent_email"] <- as.factor(treino_fac[,"sent_email"])
treino_fac[,"to_multiple"] <- as.factor(treino_fac[,"to_multiple"])
treino_fac[,"format"] <- as.factor(treino_fac[,"format"])
treino_fac[,"re_subj"] <- as.factor(treino_fac[,"re_subj"])

teste_fac <- teste
teste_fac[,"sent_email"] <- as.factor(teste_fac[,"sent_email"])
teste_fac[,"to_multiple"] <- as.factor(teste_fac[,"to_multiple"])
teste_fac[,"format"] <- as.factor(teste_fac[,"format"])
teste_fac[,"re_subj"] <- as.factor(teste_fac[,"re_subj"])
