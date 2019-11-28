setwd("C:/Users/MARCELLO/Desktop/ENG436/topicos_eng436")

#Analise supervisionada

##Bibliotecas----

if (!require("pacman")) install.packages("pacman")

pacman::p_load(dplyr, tidyr, ggplot2, caret, cowplot, 
               randomForest, reshape2, multiROC, readxl, plotly, 
               tibble, GGally, lemon, factoextra, e1071)

##Manipulando o banco de dados----

dataset <- readxl::read_excel("glass.xlsx")
dplyr::glimpse(dataset)

df <- dataset %>% 
  dplyr::mutate_if(is.character, as.numeric) %>% 
  dplyr::mutate(Tipo = as.factor(Type)) %>% 
  dplyr::mutate(RI = RI/1e5) %>% 
  dplyr::select(-Type)

levels(df$Tipo) <- make.names(levels(factor(df$Tipo)))

dplyr::glimpse(df)
table(df$Tipo)

##Outliers----

df <- df %>% 
  tibble::rowid_to_column() %>% 
  dplyr::select(1:11)

df1 <- df %>% 
  dplyr::filter(rowid %in% c("107","108","164","173","175"))%>% 
  dplyr::mutate(Out = 1)

df <- df1 %>% 
  dplyr::right_join(df, by = "rowid") %>% 
  dplyr::select(c(1, 12:22)) %>% 
  dplyr::mutate(Out = ifelse(is.na(Out), 0, 1)) %>% 
  dplyr::select(1, 3:12, 2) %>% 
  dplyr::mutate(Out = as.factor(Out)) %>% 
  dplyr::filter(Out==0) %>% 
  dplyr::select(c(2:11))

colnames(df) <- c("RI", "Na","Mg","Al","Si","K","Ca","Ba","Fe","Tipo")

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
                         ntree = 1500,
                         preProcess = c("scale", "center"),
                         trControl = trainControl(method  = "cv", 
                                                   number  = 10,
                                                   classProbs = T,
                                                   savePredictions = T,
                                                   verboseIter = FALSE))
                                                  
                         
predictions <- predict(model_rf,test_data)
cm_original <- confusionMatrix(predictions, test_data$Tipo)

##RF Resample----

###UP

set.seed(42)
model_rf_up <- caret::train(Tipo ~ .,
                         data = train_data,
                         method = "rf",
                         ntree = 1500,
                         preProcess = c("scale", "center"),
                         trControl = trainControl(method  = "cv", 
                                                  number  = 10,
                                                  classProbs = T,
                                                  savePredictions = T,
                                                  verboseIter = FALSE,
                                                  sampling = "up"))


predictions_up <- predict(model_rf_up,test_data)
cm_up <- confusionMatrix(predictions_up, test_data$Tipo)

###DOWN

set.seed(42)
model_rf_down <- caret::train(Tipo ~ .,
                            data = train_data,
                            method = "rf",
                            ntree = 1500,
                            preProcess = c("scale", "center"),
                            trControl = trainControl(method  = "cv", 
                                                     number  = 10,
                                                     classProbs = T,
                                                     savePredictions = T,
                                                     verboseIter = FALSE,
                                                     sampling = "down"))


predictions_down <- predict(model_rf_down,test_data)
cm_down <- confusionMatrix(predictions_down, test_data$Tipo)

##Comparando RF's----

#1) Ss, Sp e F1

df_rf_original <- data.frame(Modelo = "Original",
                          Sensitividade = cm_original$byClass[1:6],
                          Especificidade = cm_original$byClass[7:12],
                          F1 = cm_original$byClass[37:42])

df_rf_down <- data.frame(Modelo = "Under",
                             Sensitividade = cm_down$byClass[1:6],
                             Especificidade = cm_down$byClass[7:12],
                             F1 = cm_down$byClass[37:42])

df_rf_up <- data.frame(Modelo = "Over",
                             Sensitividade = cm_up$byClass[1:6],
                             Especificidade = cm_up$byClass[7:12],
                             F1 = cm_up$byClass[37:42])

#2) Acc, Kappa

df_rf_original1 <- data.frame(Modelo = "Original",
                              Acuracia = cm_original$overall[1],
                              Kappa = cm_original$overall[2])

df_rf_down1 <- data.frame(Modelo = "Under",
                                  Acuracia = cm_down$overall[1],
                                  Kappa = cm_down$overall[2])
df_rf_up1 <- data.frame(Modelo = "Over",
                                 Acuracia = cm_up$overall[1],
                                 Kappa = cm_up$overall[2])

###Data.frame das metricas

models <- df_rf_original %>% 
  dplyr::bind_rows(df_rf_up, df_rf_down) %>% 
  mutate(Classe = rep(paste0(rep(paste0("X"), 5),setdiff(1:7,4)), 3),
         Classe=as.factor(Classe))

models1 <- df_rf_original1 %>% 
  dplyr::bind_rows(df_rf_up1, df_rf_down1)

###PLOT Ss, Sp, F1

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

###PLOT Acc, Kappa

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

###DOWN

predictions_down1 <- predict(model_rf_down,test_data,type="prob")
colnames(predictions_down1) <- paste(colnames(predictions_down1), "_pred_down")

###Juntando

true_label <- dummies::dummy(test_data$Tipo, sep = ".")
true_label <- data.frame(true_label)
colnames(true_label) <- gsub(".*?\\.", "", colnames(true_label))
colnames(true_label) <- paste(colnames(true_label), "_true")
final_df <- cbind(true_label, predictions1, predictions_up1,predictions_down1)

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
  scale_alpha(range=c(0.05, 1))+
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

##OOB----

###ORIGINAL

ccc <- model_rf$finalModel
ccc <- as.data.frame(ccc[["err.rate"]])

df_oob <- ccc %>% 
  dplyr::mutate(tree = 1:1500) %>% 
  reshape2::melt(id.vars = "tree") %>% 
  mutate(alpha=ifelse(variable=="OOB",1,0))

plot_oob <- ggplot2::ggplot(df_oob) +
  geom_line(aes(x = tree, y = value, col = variable,alpha=alpha), 
            size = 1) +
  xlab("Num. de Arvores") + 
  ylab("Erro") +
  labs(col = "Legenda") +
  theme(legend.position = "bottom")+
  scale_alpha(range=c(0.25, 1))

plotly::ggplotly(plot_oob)

###UP

ccc_up <- model_rf_up$finalModel
ccc_up <- as.data.frame(ccc_up[["err.rate"]])

df_oob_up <- ccc_up %>% 
  dplyr::mutate(tree = 1:1500) %>% 
  reshape2::melt(id.vars = "tree") %>% 
  mutate(alpha=ifelse(variable=="OOB",1,0))

plot_oob_up <- ggplot2::ggplot(df_oob_up) +
  geom_line(aes(x = tree, y = value, col = variable,alpha=alpha), 
            size = 1) +
  xlab("Num. de Arvores") + 
  ylab("Erro") +
  labs(col = "Legenda") +
  theme(legend.position = "bottom")+
  scale_alpha(range=c(0.25, 1))

plotly::ggplotly(plot_oob_up)

###DOWN

ccc_down <- model_rf_down$finalModel
ccc_down <- as.data.frame(ccc_down[["err.rate"]])

df_oob_down <- ccc_down %>% 
  dplyr::mutate(tree = 1:1500) %>% 
  reshape2::melt(id.vars = "tree") %>% 
  mutate(alpha=ifelse(variable=="OOB",1,0))

plot_oob_down <- ggplot2::ggplot(df_oob_down) +
  geom_line(aes(x = tree, y = value, col = variable,alpha=alpha), 
            size = 1) +
  xlab("Num. de Arvores") + 
  ylab("Erro") +
  labs(col = "Legenda") +
  theme(legend.position = "bottom")+
  scale_alpha(range=c(0.25, 1))

plotly::ggplotly(plot_oob_down)

##Plots f(mtry)----

###ORIGNAL

g4 <- ggplot(model_rf$results) +
  geom_line(aes(x = mtry, y = Accuracy)) +
  geom_point(aes(x = mtry, y = Accuracy)) +
  geom_errorbar(aes(x = mtry, ymin = Accuracy - AccuracySD, ymax = Accuracy + AccuracySD), width = 0.3)+
  theme_bw()
g4

g5 <- ggplot(model_rf$results) +
  geom_line(aes(x = mtry, y = Kappa)) +
  geom_point(aes(x = mtry, y = Kappa)) +
  geom_errorbar(aes(x = mtry, ymin = Kappa - KappaSD, ymax = Kappa + KappaSD), width = 0.3)+
  theme_bw()
g5

###UP

g6 <- ggplot(model_rf_up$results) +
  geom_line(aes(x = mtry, y = Accuracy)) +
  geom_point(aes(x = mtry, y = Accuracy)) +
  geom_errorbar(aes(x = mtry, ymin = Accuracy - AccuracySD, ymax = Accuracy + AccuracySD), width = 0.3)+
  theme_bw()
g6

g7 <- ggplot(model_rf_up$results) +
  geom_line(aes(x = mtry, y = Kappa)) +
  geom_point(aes(x = mtry, y = Kappa)) +
  geom_errorbar(aes(x = mtry, ymin = Kappa - KappaSD, ymax = Kappa + KappaSD), width = 0.3)+
  theme_bw()
g7

###DOWN

g8 <- ggplot(model_rf_down$results) +
  geom_line(aes(x = mtry, y = Accuracy)) +
  geom_point(aes(x = mtry, y = Accuracy)) +
  geom_errorbar(aes(x = mtry, ymin = Accuracy - AccuracySD, ymax = Accuracy + AccuracySD), width = 0.3)+
  theme_bw()
g8

g9 <- ggplot(model_rf_down$results) +
  geom_line(aes(x = mtry, y = Kappa)) +
  geom_point(aes(x = mtry, y = Kappa)) +
  geom_errorbar(aes(x = mtry, ymin = Kappa - KappaSD, ymax = Kappa + KappaSD), width = 0.3)+
  theme_bw()
g9