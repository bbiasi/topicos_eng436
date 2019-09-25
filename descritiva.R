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


