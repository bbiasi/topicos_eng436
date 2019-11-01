#Análise não supervisionada

setwd("C:/Users/MARCELLO/Desktop/ENG436/topicos_eng436")

if (!require("pacman")) install.packages("pacman")

pacman::p_load(dplyr, readxl, ggplot2, cowplot, plotly, 
               GGally, tibble, clValid, lemon, randomForest, caret)

## Manipulando o banco de dados----
dataset <- readxl::read_excel("glass.xlsx")
dplyr::glimpse(dataset)

df <- dataset %>% 
  dplyr::mutate_if(is.character, as.numeric) %>% 
  dplyr::mutate(Tipo = as.factor(Type)) %>% 
  dplyr::select(-Type)

levels(df$Tipo) <- make.names(levels(factor(df$Tipo)))

dplyr::glimpse(df)
table(df$Tipo)

## Normalizacao de dados----

# normalize <- function(x) {
#   return ((x - min(x)) / (max(x) - min(x)))
# }
# 
# df$Na_norm <- normalize(df$Na)
# df$Mg_norm <- normalize(df$Mg)
# df$Al_norm <- normalize(df$Al)
# df$Si_norm <- normalize(df$Si)
# df$K_norm <- normalize(df$K)
# df$Ca_norm <- normalize(df$Ca)
# df$Ba_norm <- normalize(df$Ba)
# df$Fe_norm <- normalize(df$Fe)
# df$RI_norm <- normalize(df$Na)

## Analise de Outliers----

# Possiveis outliers:

df <- df %>% 
  tibble::rowid_to_column() %>% 
  dplyr::select(1:11) 

# outliers identificados visualmente
df1 <- df %>% 
  dplyr::filter(rowid %in% c("107","173","175","164","108")) %>% 
  dplyr::mutate(Out = 1)

df2 <- df1 %>% 
  dplyr::right_join(df, by = "rowid") %>% 
  dplyr::select(c(1, 12:22)) %>% 
  dplyr::mutate(Out = ifelse(is.na(Out), 0, 1)) %>% 
  dplyr::select(1, 3:12, 2) %>% 
  dplyr::mutate(Out = as.factor(Out))

boxplot_1 <- ggplot(df2)+
  geom_boxplot(aes(y = Na.y,x = Tipo.y),
               color = "black", show.legend = F) +
  geom_jitter(aes(y = Na.y,x = Tipo.y,
                  shape = Out, fill = Tipo.y,
                  size = Out),
              alpha = 0.5) +
  scale_shape_manual(values = c(24, 21)) +
  xlab(label = "Tipo") +
  ylab(label="%")

boxplot_1

ggplotly(boxplot_1)

df3 <- df2 %>% 
  dplyr::filter(!(rowid %in% c("107","164","108"))) %>% 
  dplyr::select(2:10)

df4 <- df3

df3 <- df3 %>% 
  na.omit() %>% 
  base::scale()

cmin <- 2
cmax <- 7
algoritmos <- c("hierarchical","kmeans","pam")
intern <- clValid::clValid(df3, cmin:cmax, 
                           clMethods = algoritmos, validation = "internal")
summary(intern)   

op <- par(no.readonly = TRUE)
par(mfrow = c(2, 2), mar = c(4, 4, 3, 1))
plot(intern, legend = FALSE)
plot(nClusters(intern), measures(intern, "Dunn")[, , 1], type = "n",
     axes = F, xlab = "", ylab = "")
legend("center", clusterMethods(intern), col = 1:3, lty = 1:3,
       pch = paste(1:3)) ; par(op)

estabilidade <- clValid::clValid(df3, cmin:cmax,
                                 clMethods = algoritmos,
                                 validation = "stability")
summary(estabilidade)

op <-  par(mfrow = c(2, 2), mar = c(4, 4, 3, 1))
plot(estabilidade, measure = c("APN", "AD", "ADM","FOM"), legend = FALSE)
plot(nClusters(estabilidade), measures(estabilidade, "APN")[, , 1], type = "n",
     + axes = F, xlab = "", ylab = "")
egend("center", clusterMethods(stab), col = 1:3, lty = 1:3,
      + pch = paste(1:3))
par(op)

vidro.Dend2 <- df3 %>% 
  stats::dist(method   = "euclidean") %>% 
  stats::hclust(method = "ward.D")

opt0L <- theme_bw() +
  theme(axis.title   = element_text(face = "bold", color  = "black", size = 26),
        axis.text.x  = element_text(face = "bold", color = "black", 
                                    size  = 18, angle = 90),
        axis.text.y  = element_text(face = "bold", color = "black", size = 22),
        legend.text  = element_text(size = 18, face = "bold"),
        legend.title = element_text(size = 30, face = "bold"),
        strip.text.x = element_text(size = 18, face = "bold"),
        strip.text.y = element_text(size = 14, face = "bold"))

# dend_ch <- factoextra::fviz_dend(vidro.Dend2, k = 2, cex = 0,
#                                  k_colors = c("black", "black"),
#                                  main  = "Dendrograma - Vidro",
#                                  ylab  = "Distâncias", sub = "",
#                                  xlab  = "Objetos",
#                                  lwd   = 1) +
#   # geom_hline(yintercept = 80, linetype = 2, size = 1) +
#   scale_x_continuous(expand = c(0, 0)) +
#   scale_y_continuous(expand = c(0, 0)) +
#   opt0L +
#   theme(axis.text.x  = element_blank(),
#         axis.ticks.x = element_blank(),
#         plot.title = element_text(hjust = 0.5, size = 30, face = "bold"), 
#         panel.border = element_blank(),
#         panel.background = element_blank(),
#         panel.grid.major.y = element_blank(),
#         panel.grid.minor.y = element_blank(),
#         panel.grid.major.x = element_blank(),
#         panel.grid.minor.x = element_blank())
# 
# dend_ch

dend_ch2 <- factoextra::fviz_dend(vidro.Dend2, k = 2, cex = 0,
                                  k_colors = c("darkblue", "darkgreen"),
                                  main  = "Dendrograma - Vidro",
                                  ylab  = "Distâncias", sub = "",
                                  xlab  = "Objetos",
                                  lwd   = 1) +
  geom_hline(yintercept = 80, linetype = 2, size = 1) +
  scale_x_continuous(expand = c(0, 0)) +
  scale_y_continuous(expand = c(0, 0)) +
  opt0L +
  theme(axis.text.x  = element_blank(),
        axis.ticks.x = element_blank(),
        plot.title = element_text(hjust = 0.5, size = 30, face = "bold"), 
        panel.border = element_blank(),
        panel.background = element_blank(),
        panel.grid.major.y = element_blank(),
        panel.grid.minor.y = element_blank(),
        panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank())

dend_ch2

# ggplotly(dend_ch2)

# Trnsformndo predidotores importantes
# pag 154 - islr

# rf n supervisionada
# https://nishanthu.github.io/articles/ClusteringUsingRandomForest.html

{
  set.seed(1)
  rf.fit <- randomForest::randomForest(x = df4, 
                                       y = NULL, 
                                       ntree = 1500, 
                                       proximity = TRUE, 
                                       oob.prox  = TRUE)
}

plot(caret::varImp(rf.fit))

importancia_nsup <- rf.fit$importance

importancia_nsup <- importancia_nsup %>% 
  as.data.frame() %>% 
  tibble::rownames_to_column()

importancia_nsup$rowname <- gsub('.{2}$', '', importancia_nsup$rowname)


# https://stats.stackexchange.com/questions/197827/how-to-interpret-mean-decrease-in-accuracy-and-mean-decrease-gini-in-random-fore

gini <- importancia_nsup %>% 
  dplyr::arrange(MeanDecreaseGini) %>% 
  ggplot2::ggplot() +
  geom_segment(aes(x = reorder(rowname, -MeanDecreaseGini),
                   y = 0, 
                   xend = rowname, 
                   yend = MeanDecreaseGini), color = "grey50") +
  geom_point(aes(x =  reorder(rowname, -MeanDecreaseGini), 
                 y = MeanDecreaseGini), 
             col = "black", show.legend = F) +
  coord_flip() +
  scale_fill_grey() +
  xlab("Variáveis") +
  ggtitle("Random Forest - Não Supervisionada") 

gini


df3 <- df4 %>% 
  dplyr::rename("RI" = RI.y, "Na" = Na.y, "Mg" = Mg.y,
                "Al" = Al.y, "Si" = Si.y, "K"  = K.y, 
                "Ca" = Ca.y, "Ba" = Ba.y, "Fe" = Fe.y) %>% 
  dplyr::mutate(Si_2 = Si^2,
                Mg_2 = Mg^2,
                RI_2 = RI^2,
                Na_2 = Na^2,
                K_2 = K^2,
                Al_2 = Al^2,
                Ca_2 = Ca^2, Ca_3 = Ca^3)

df3 <- df3 %>% 
  na.omit() %>% 
  base::scale()

# Veriricando clusters ----

vidro_df <- df3 %>% 
  stats::dist(method   = "euclidean") %>% 
  stats::hclust(method = "ward.D")

vidro_df2 <- vidro_df %>% 
  stats::cutree(k = 2) %>% 
  data.frame()

colnames(vidro_df2) <- "cluster2"

clust_2 <- dfx %>% 
  dplyr::mutate(cluster2 = vidro_df2$cluster2)

clust_2 <- clust_2 %>% 
  dplyr::filter(!(rowid %in% c("106","109","110", "111", "112", "113", "131", "132",
                               "188",
                               "175")))

# FILTRO REALIZADO COM BASE AO NAO PERTENCIMENTO DO CLUSTER BINOMIAL
dfx2 <- df2 %>% 
  dplyr::filter(!(rowid %in% c("106","109","110", "111", "112", "113", "131", "132",
                               "188",
                               "175",
                               "107", "108", "164"))) %>% # df3
  dplyr::rename("RI" = RI.y, "Na" = Na.y, "Mg" = Mg.y,
                "Al" = Al.y, "Si" = Si.y, "K"  = K.y, 
                "Ca" = Ca.y, "Ba" = Ba.y, "Fe" = Fe.y,
                "CLASSE" = Tipo.y) %>% 
  dplyr::mutate(Si_2 = Si^2,
                Mg_2 = Mg^2,
                RI_2 = RI^2,
                Na_2 = Na^2,
                K_2 = K^2,
                Al_2 = Al^2,
                Ca_2 = Ca^2, Ca_3 = Ca^3,
                CLASSE = CLASSE) %>% 
  dplyr::select(-Out)

marcelo <- dfx2 %>% 
  dplyr::select(-c(rowid))

save(marcelo, dfx2, file = "karla.RData")

{
  set.seed(1)
  index <- caret::createDataPartition(marcelo$CLASSE, p = 0.7, list = FALSE) 
  train <- marcelo[index, ]
  test  <- marcelo[-index, ]
  
  ctrl  <- caret::trainControl(method  = "cv", 
                               number  = 10,
                               classProbs = T,
                               savePredictions = T,
                               verboseIter = FALSE)
  
  model1_orig_rf <- caret::train(CLASSE ~ .,
                                 data   = train,
                                 method = "rf",
                                 preProcess = c("scale", "center"),
                                 metric     = "Accuracy",
                                 trControl  = ctrl,
                                 ntree = 1500)
}

predictions <- predict(model1_orig_rf, test)
cm_original <- confusionMatrix(predictions, test$CLASSE)
cm_original


# ABORDAGEM BINOMIAL ----

DF_BIN <- marcelo %>% 
  dplyr::mutate(cluster = clust_2$cluster2,
                cluster = ifelse(cluster == 1, "c1", "c2"),
                cluster = as.factor(cluster)) %>% 
  dplyr::select(-CLASSE)


{
  set.seed(1)
  index <- caret::createDataPartition(DF_BIN$cluster, p = 0.7, list = FALSE) 
  train <- DF_BIN[index, ]
  test  <- DF_BIN[-index, ]
  
  ctrl  <- caret::trainControl(method  = "cv", 
                               number  = 10,
                               classProbs = T,
                               savePredictions = T,
                               verboseIter = FALSE)
  
  model1_orig_rf <- caret::train(cluster ~ .,
                                 data   = train,
                                 method = "rf",
                                 preProcess = c("scale", "center"),
                                 metric     = "Accuracy",
                                 trControl  = ctrl,
                                 ntree = 1500)
  }

predictions <- predict(model1_orig_rf, test)
cm_original <- confusionMatrix(predictions, test$cluster)
cm_original

# SEPERANDO OS CLUSTERS 01 E CLUSTER 02

DF_C1 <- marcelo %>% 
  dplyr::mutate(cluster = clust_2$cluster2,
                cluster = ifelse(cluster == 1, "c1", "c2"),
                cluster = as.factor(cluster)) %>% 
  dplyr::filter(cluster == "c1") %>% 
  dplyr::mutate(CLASSE = droplevels(DF_C1$CLASSE)) %>% 
  dplyr::select(-cluster)

levels(DF_C1$CLASSE)

DF_C2 <- marcelo %>% 
  dplyr::mutate(cluster = clust_2$cluster2,
                cluster = ifelse(cluster == 1, "c1", "c2"),
                cluster = as.factor(cluster)) %>% 
  dplyr::filter(cluster == "c2") %>% 
  # dplyr::mutate(CLASSE = as.factor(CLASSE)) %>% 
  dplyr::mutate(CLASSE = droplevels(DF_C2$CLASSE)) %>% 
  dplyr::select(-cluster)


# cluster 01
{
  set.seed(1)
  index <- caret::createDataPartition(DF_C1$CLASSE, p = 0.7, list = FALSE) 
  train <- DF_C1[index, ]
  test  <- DF_C1[-index, ]
  
  ctrl  <- caret::trainControl(method  = "cv", 
                               number  = 10,
                               classProbs = T,
                               savePredictions = T,
                               verboseIter = FALSE)
  
  model1_orig_rf <- caret::train(CLASSE ~ .,
                                 data   = train,
                                 method = "rf",
                                 preProcess = c("scale", "center"),
                                 metric     = "Accuracy",
                                 trControl  = ctrl,
                                 ntree = 1500)
  }

predictions <- predict(model1_orig_rf, test)
cm_original <- confusionMatrix(predictions, test$CLASSE)
cm_original

# cluster 02
{
  set.seed(1)
  index <- caret::createDataPartition(DF_C2$CLASSE, p = 0.7, list = FALSE) 
  train <- DF_C2[index, ]
  test  <- DF_C2[-index, ]
  
  ctrl  <- caret::trainControl(method  = "cv", 
                               number  = 10,
                               classProbs = T,
                               savePredictions = T,
                               verboseIter = FALSE)
  
  model1_orig_rf <- caret::train(CLASSE ~ .,
                                 data   = train,
                                 method = "rf",
                                 preProcess = c("scale", "center"),
                                 metric     = "Accuracy",
                                 trControl  = ctrl,
                                 ntree = 1500)
}

predictions <- predict(model1_orig_rf, test)
cm_original <- confusionMatrix(predictions, test$CLASSE)
cm_original


# h2o ----
# 
# 
# library(h2o)
# 
# h2o::h2o.init()
# 
# # Problema com JDK
# # https://stackoverflow.com/a/27667945/9699371
# 
# # Sys.setenv(JAVA_HOME = 'C:/Program Files/Java/jdk1.8.0_221/jre')
# # https://stackoverflow.com/questions/5085774/how-to-modify-and-save-rprofile-site-under-windows
# 
# teste_super <- as.h2o(
#   marcelo
# )
# 
# 
# splits <- h2o.splitFrame(teste_super, ratios = 0.7, seed = 1)
# train  <- h2o.assign(splits[[1]], "train")   
# test <- h2o.assign(splits[[2]], "test")
# 
# colnames(teste_super)
# 
# y <- "CLASSE"
# x <- setdiff(names(train), y)
# 
# train[,y] <- as.factor(train[,y])
# test[,y]  <- as.factor(test[,y])
# 
# 
# nfolds <- 10
# 
# 
# my_gbm <- h2o.gbm(x = x,
#                   y = y,
#                   training_frame = train,
#                   distribution = "multinomial",
#                   ntrees = 500,
#                   max_depth = 3,
#                   min_rows = 2,
#                   learn_rate = 0.1,
#                   nfolds = nfolds,
#                   fold_assignment = "Modulo",
#                   keep_cross_validation_predictions = TRUE,
#                   seed = 1)
# 
# # RF
# my_rf <- h2o.randomForest(x = x,
#                           y = y,
#                           training_frame = train,
#                           ntrees = 500,
#                           nfolds = nfolds,
#                           fold_assignment = "Modulo",
#                           keep_cross_validation_predictions = TRUE,
#                           seed = 1)
# 
# ensemble <- h2o.stackedEnsemble(x = x,
#                                 y = y,
#                                 training_frame = train,
#                                 model_id = "my_ensemble_binomial",
#                                 base_models = list(my_gbm, my_rf))
# 
# perf <- h2o.performance(ensemble, newdata = test)
# 
# perf_gbm_test <- h2o.performance(my_gbm, newdata = test)
# perf_rf_test  <- h2o.performance(my_rf, newdata = test)
# 
# baselearner_best_auc_test <- max(h2o.auc(perf_gbm_test), h2o.auc(perf_rf_test))
# 
# 
# ensemble_auc_test <- h2o.auc(perf)
# 
# 
# print(sprintf("Best Base-learner Test AUC:  %s", baselearner_best_auc_test))
# print(sprintf("Ensemble Test AUC:  %s", ensemble_auc_test))

# https://rpubs.com/chidungkt/449576