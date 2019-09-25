setwd("D:/PEI/Doc/Disciplinas/Topicos/topicos_eng436")

library(dplyr)
library(readxl)

# Carregando ds e realizando manipulacoes basicas ----
dataset <- readxl::read_excel("glass.xlsx")
dplyr::glimpse(dataset)

df <- dataset %>% 
  dplyr::mutate_if(is.character, as.numeric) %>% 
  dplyr::mutate(Tipo = as.factor(Type)) %>% 
  dplyr::select(-Type)

levels(df$Tipo) <- make.names(levels(factor(df$Tipo)))

dplyr::glimpse(df)
table(df$Tipo)



{
  set.seed(1)
  index <- caret::createDataPartition(df$Tipo, p = 0.75, list = FALSE) 
  train <- df[index, ]
  test  <- df[-index, ]
  
  ctrl  <- caret::trainControl(method  = "cv", 
                               number  = 10,
                               classProbs = T,
                               savePredictions = T,
                               verboseIter = FALSE,
                               summaryFunction = specmine::multiiClassSummary)
  
  model1_r_rf <- caret::train(y = train$Tipo,
                              x = train[, "Tipo"],
                              method = "rf",
                              preProcess = c("scale", "center"),
                              metric     = "Accuracy",
                              trControl  = ctrl,
                              ntree = 1500)
}

model1_r_rf
