setwd("C:/Users/MARCELLO/Desktop/ENG436/topicos_eng436")

#Análise supervisionada

##Bibliotecas----

library(readxl)
library(dplyr)
library(ggplot2)
library(cowplot)
library(plotly)
library(GGally)
library(tibble)
library(clValid)
library(lemon)
library(factoextra)
library(caret)
library(e1071)
library(randomForest)

##Manipulando o banco de dados----

dataset <- readxl::read_excel("glass.xlsx")
dplyr::glimpse(dataset)

df <- dataset %>% 
  dplyr::mutate_if(is.character, as.numeric) %>% 
  dplyr::mutate(Tipo = as.factor(Type)) %>% 
  dplyr::select(-Type)

levels(df$Tipo) <- make.names(levels(factor(df$Tipo)))

dplyr::glimpse(df)
table(df$Tipo)

##Teste-Treino----

set.seed(42)
index <- createDataPartition(df$Tipo, p = 0.7, list = FALSE)
train_data <- df[index, ]
test_data  <- df[-index, ]

##RF----

set.seed(42)
model_rf <- caret::train(Tipo ~ .,
                         data = train_data,
                         method = "rf",
                         preProcess = c("scale", "center"),
                         trControl = trainControl(method  = "cv", 
                                                   number  = 10,
                                                   classProbs = T,
                                                   savePredictions = T,
                                                   verboseIter = FALSE))
                                                  
                         
predictions <- predict(model_rf,test_data)
cm_original <- confusionMatrix(predictions, test_data$Tipo)
  
##KNN----

set.seed(42)
model_knn <- caret::train(Tipo ~ .,
                         data = train_data,
                         method = "knn",
                         preProcess = c("scale", "center"),
                         trControl = trainControl(method  = "cv", 
                                                  number  = 10,
                                                  classProbs = T,
                                                  savePredictions = T,
                                                  verboseIter = FALSE))


predictions_knn <- predict(model_knn,test_data)
cm_original_knn <- confusionMatrix(predictions_knn, test_data$Tipo)

##RF Resample----

###UP

set.seed(42)
model_rf_up <- caret::train(Tipo ~ .,
                         data = train_data,
                         method = "rf",
                         preProcess = c("scale", "center"),
                         trControl = trainControl(method  = "cv", 
                                                  number  = 10,
                                                  classProbs = T,
                                                  savePredictions = T,
                                                  verboseIter = FALSE,
                                                  sampling = "up"))


predictions_up <- predict(model_rf_up,test_data)
cm_original_up <- confusionMatrix(predictions_up, test_data$Tipo)

###DOWN

set.seed(42)
model_rf_down <- caret::train(Tipo ~ .,
                            data = train_data,
                            method = "rf",
                            preProcess = c("scale", "center"),
                            trControl = trainControl(method  = "cv", 
                                                     number  = 10,
                                                     classProbs = T,
                                                     savePredictions = T,
                                                     verboseIter = FALSE,
                                                     sampling = "down"))


predictions_down <- predict(model_rf_down,test_data)
cm_original_down <- confusionMatrix(predictions_down, test_data$Tipo)

##Comparação RF's----

df_rf_original <- data.frame(Acuracia = cm_original$overall[1],
                          kappa  = cm_original$overall[2],
                          Sentit = cm_original$byClass[1],
                          Especi = cm_original$byClass[2],
                          F1 = cm_original$byClass[7])

df_rf_original_down <- data.frame(Acuracia = cm_original_down$overall[1],
                             kappa  = cm_original_down$overall[2],
                             Sentit = cm_original_down$byClass[1],
                             Especi = cm_original_down$byClass[2],
                             F1 = cm_original_down$byClass[7])

df_rf_original_up <- data.frame(Acuracia = cm_original_up$overall[1],
                             kappa  = cm_original_up$overall[2],
                             Sentit = cm_original_up$byClass[1],
                             Especi = cm_original_up$byClass[2],
                             F1 = cm_original_up$byClass[7])

models <- df_rf_original %>% 
  dplyr::bind_rows(df_rf_original_up, df_rf_original_down)

