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


library(skimr)
skimmed <- skim_to_wide(df)
skimmed[, c(1:5, 9:11, 13, 15:16)]
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
                         ntree = 500,
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
                              Acuracia = cm_original$overall[1],
                              Kappa = cm_original$overall[2])

df_rf_original_down1 <- data.frame(Modelo = "Under",
                                  Acuracia = cm_original_down$overall[1],
                                  Kappa = cm_original_down$overall[2])
df_rf_original_up1 <- data.frame(Modelo = "Over",
                                 Acuracia = cm_original_up$overall[1],
                                 Kappa = cm_original_up$overall[2])

###Data.frame das m?tricas

models <- df_rf_original %>% 
  dplyr::bind_rows(df_rf_original_up, df_rf_original_down) %>% 
  mutate(Classe = rep(paste0(rep(paste0("X"), 5),setdiff(1:7,4)), 3),
         Classe=as.factor(Classe))

models1 <- df_rf_original1 %>% 
  dplyr::bind_rows(df_rf_original_up1, df_rf_original_down1)

###PLOT DAS M?TRICAS Ss, Sp, F1

g1 <- models %>% 
  reshape2::melt(id.vars=c("Modelo","Classe")) %>%
  ggplot(aes(x = variable, y = value, color = Modelo)) +
  geom_jitter(width = 0.2, alpha = 1, size = 5)+
  facet_wrap(~Classe,ncol = length(levels(models$Classe)))+
  theme(axis.text.x  = element_text(angle = 90),legend.position = "bottom")+
  theme(axis.title.x = element_text(face = "bold"),
        axis.title.y = element_text(face = "bold"),
        axis.text = element_text(face = "bold"))+
  xlab("Variavel")+
  ylab("Valor")
plotly::ggplotly(g1)

ggsave(filename = "metricas_classe.png",plot = g1,width = 13,height = 7,dpi = 300)
###PLOT DAS M?TRICAS Acc, Kappa

g2 <- models1 %>%
  gather(x, y, Acuracia:Kappa) %>% 
  ggplot(aes(x = x, y = y, color = Modelo)) +
  geom_jitter(width = 0.2, alpha = 1, size = 5)+
  theme(axis.text.x  = element_text(angle = 90),legend.position = "bottom")+
  theme(axis.title.x = element_text(face = "bold"),
        axis.title.y = element_text(face = "bold"),
        axis.text = element_text(face = "bold"))+
  xlab("Variavel")+
  ylab("Valor")
plotly::ggplotly(g2)

ggsave(filename = "metricas_geral.png",plot = g2,width = 13,height = 7,dpi = 300)

##CURVA ROC----

###Original 

predictions1 <- predict(model_rf,test_data,type="prob")
colnames(predictions1) <- paste(colnames(predictions1), "_pred_original")

###Up

predictions_up1 <- predict(model_rf_up,test_data,type="prob")
colnames(predictions_up1) <- paste(colnames(predictions_up1), "_pred_up")
###Juntando

true_label <- dummies::dummy(test_data$Tipo, sep = ".")
true_label <- data.frame(true_label)
colnames(true_label) <- gsub(".*?\\.", "", colnames(true_label))
colnames(true_label) <- paste(colnames(true_label), "_true")
final_df <- cbind(true_label, predictions1, predictions_up1)

###PLotando curva

roc_res <- multi_roc(final_df, force_diag=T)
pr_res <- multi_pr(final_df, force_diag=T)
plot_roc_df <- plot_roc_data(roc_res)
plot_pr_df <- plot_pr_data(pr_res)
plot_roc_df <- plot_roc_df %>% 
  mutate(alpha=ifelse(plot_roc_df$Group=="Micro"|plot_roc_df$Group=="Macro",1,0))

g3 <- ggplot(plot_roc_df, aes(x = 1-Specificity, y=Sensitivity)) +
  geom_path(aes(color = Group, linetype=Method,alpha=alpha), size=1.5) +
  geom_segment(aes(x = 0, y = 0, xend = 1, yend = 1), 
               colour='grey', linetype = 'dotdash') +
  scale_alpha(range=c(0.1, 1))+
  xlab("1-Especificidade")+
  ylab("Sensitividade")
  theme_bw() + 
  theme(plot.title = element_text(hjust = 0.5), 
        legend.justification=c(1, 0), legend.position=c(.95, .05),
        legend.title=element_blank(), 
        legend.background = element_rect(fill=NULL, size=0.5, 
                                         linetype="solid", colour ="black"))
plotly::ggplotly(g3)
ggsave(filename = "curva_roc.png",plot = g3,width = 13,height = 7,dpi = 300)

###OOB

ccc <- model_rf_up$finalModel
ccc <- as.data.frame(ccc[["err.rate"]])

df_oob <- ccc %>% 
  dplyr::mutate(tree = 1:500) %>% 
  reshape2::melt(id.vars = "tree") %>% 
  mutate(alpha=ifelse(variable=="OOB",1,0))

# OOB com ggplot2

plot_oob <- ggplot2::ggplot(df_oob) +
  geom_line(aes(x = tree, y = value, col = variable,alpha=alpha), 
            size = 1) +
  xlab("N° Árvores") + 
  ylab("Erro") +
  labs(col = "Legenda") +
  theme(legend.position = "bottom")+
  scale_alpha(range=c(0.25, 1))

plotly::ggplotly(plot_oob)
plot_oob

#df_oob_mean <- ccc %>% 
 # dplyr::mutate(tree = 1:500) %>% 
  #reshape2::melt(id.vars = "tree") %>% 
  #filter(variable!="OOB") %>% 
  #group_by(tree) %>% 
  #summarise(Média=mean(value))
