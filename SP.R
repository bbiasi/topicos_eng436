setwd("C:/Users/MARCELLO/Desktop/ENG436/topicos_eng436")

#Análise supervisionada

##Manipulando o banco de dados----

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

dataset <- readxl::read_excel("glass.xlsx")
dplyr::glimpse(dataset)

df <- dataset %>% 
  dplyr::mutate_if(is.character, as.numeric) %>% 
  dplyr::mutate(Tipo = as.factor(Type)) %>% 
  dplyr::select(-Type)

levels(df$Tipo) <- make.names(levels(factor(df$Tipo)))

dplyr::glimpse(df)
table(df$Tipo)

#Teste-Treino----
dfx2 <- dfx2 %>% 
  select(-rowid,-Out)
  
dfx2 <- dfx2 %>% 
  mutate(CLASSE = as.factor(CLASSE))

set.seed(42)
index <- createDataPartition(dfx2$CLASSE, p = 0.7, list = FALSE)
train_data <- dfx2[index, ]
test_data  <- dfx2[-index, ]

##RF----

set.seed(42)
model_rf <- caret::train(CLASSE ~ .,
                         data = train_data,
                         method = "rf",
                         preProcess = c("scale", "center"),
                         trControl = trainControl(method  = "cv", 
                                                   number  = 10,
                                                   classProbs = T,
                                                   savePredictions = T,
                                                   verboseIter = FALSE))
                                                  
                         
predictions <- predict(model_rf,test_data)
cm_original <- confusionMatrix(predictions, test_data$CLASSE)
  
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



###GrÁFICOS
models <- df_rf_original %>% 
  dplyr::bind_rows(df_rf_original_up, df_rf_original_down) %>% 
  reshape2::melt(id.vars = c("resamp")) %>% 
  dplyr::rename("Metrica" = variable,
                "Valor" = value) %>% 
  dplyr::mutate(resamp  = as.factor(resamp))

levels(models$resamp)  <- c("Original", "Over", "Under")
levels(models$Metrica) <- c("Acurácia", "Kappa", "Sensibilidade", 
                            "Especificidade", "F1")

pmodelo <- ggplot2::ggplot(models) +
  geom_point(aes(x = Metrica,
                 y = Valor,
                 fill = resamp),
             shape = 21, alpha = 0.7, stroke = 1.5,
             size = 5) +
  facet_wrap(~Cenario, nrow = 1) +
  scale_fill_manual(name = "Legenda",
                    values = wesanderson::wes_palette("Darjeeling1")) +
  #scale_y_continuous(limits = c(0, 1)) +
  guides(fill = guide_legend(override.aes = list(size = 4))) +
  xlab("Métrica") + ylab("Valor") +
  temag + theme(axis.text.x  = element_text(angle = 90),
                legend.position = "bottom",
                strip.text = element_text(size = 20, face = "bold"))
p_save_modeloc <- pmodelo
pmodelo
