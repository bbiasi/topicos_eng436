#Análise não supervisionada

##Manipulando o banco de dados----

library(readxl)
library(dplyr)

dataset <- readxl::read_excel("glass.xlsx")
dplyr::glimpse(dataset)

df <- dataset %>% 
  dplyr::mutate_if(is.character, as.numeric) %>% 
  dplyr::mutate(Tipo = as.factor(Type)) %>% 
  dplyr::select(-Type)

levels(df$Tipo) <- make.names(levels(factor(df$Tipo)))

dplyr::glimpse(df)
table(df$Tipo)

##Normalização de dados----

normalize <- function(x) {
  return ((x - min(x)) / (max(x) - min(x)))
}

df$Na_norm <- normalize(df$Na)
df$Mg_norm <- normalize(df$Mg)
df$Al_norm <- normalize(df$Al)
df$Si_norm <- normalize(df$Si)
df$K_norm <- normalize(df$K)
df$Ca_norm <- normalize(df$Ca)
df$Ba_norm <- normalize(df$Ba)
df$Fe_norm <- normalize(df$Fe)
df$RI_norm <- normalize(df$Na)


