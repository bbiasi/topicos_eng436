setwd("C:/Users/MARCELLO/Desktop/ENG436/topicos_eng436")

#An√°lise supervisionada

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
library(multiROC)
library(tidyr)

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

##Compara√ß√£o RF's----

#1) Ss, Sp e F1
df_rf_original <- data.frame(Modelo = "Original",
                          Sensitividade = cm_original$byClass[1:6],
                          Especificidade = cm_original$byClass[7:12],
                          F1 = cm_original$byClass[37:42])

df_rf_original_down <- data.frame(Modelo = "Under",
                             Sensitividade = cm_original_down$byClass[1:6],
                             Especificidade = cm_original_down$byClass[7:12],
                             F1 = cm_original_down$byClass[37:42])

df_rf_original_up <- data.frame(Modelo = "Over",
                             Sensitividade = cm_original_up$byClass[1:6],
                             Especificidade = cm_original_up$byClass[7:12],
                             F1 = cm_original_up$byClass[37:42])

#2) Acc, Kappa

df_rf_original1 <- data.frame(Modelo = "Original",
                              Acur·cia = cm_original$overall[1],
                              Kappa = cm_original$overall[2])

df_rf_original_down1 <- data.frame(Modelo = "Under",
                                  Acur·cia = cm_original_down$overall[1],
                                  Kappa = cm_original_down$overall[2])
df_rf_original_up1 <- data.frame(Modelo = "Over",
                                 Acur·cia = cm_original_up$overall[1],
                                 Kappa = cm_original_up$overall[2])

###Data.frame das mÈtricas

models <- df_rf_original %>% 
  dplyr::bind_rows(df_rf_original_up, df_rf_original_down) %>% 
  mutate(Classe = rep(paste0(rep(paste0("X"), 5),setdiff(1:7,4)), 3),
         Classe=as.factor(Classe))

models1 <- df_rf_original1 %>% 
  dplyr::bind_rows(df_rf_original_up1, df_rf_original_down1)

###PLOT DAS M…TRICAS Ss, Sp, F1

g1 <- models %>% 
  reshape2::melt(id.vars=c("Modelo","Classe")) %>%
  ggplot(aes(x = variable, y = value, color = Modelo)) +
  geom_jitter(width = 0.2, alpha = 0.5, size = 3)+
  facet_wrap(~Classe,ncol = length(levels(models$Classe)))+
  theme(axis.text.x  = element_text(angle = 90),legend.position = "bottom")+
  xlab("Vari·vel")+
  ylab("Valor")
plotly::ggplotly(g1)

###PLOT DAS M…TRICAS Acc, Kappa

g2 <- models1 %>%
  gather(x, y, Acur·cia:Kappa) %>% 
  ggplot(aes(x = x, y = y, color = Modelo)) +
  geom_jitter(width = 0.2, alpha = 0.5, size = 3)+
  theme(axis.text.x  = element_text(angle = 90),legend.position = "bottom")+
  xlab("Vari·vel")+
  ylab("Valor")
plotly::ggplotly(g2)
