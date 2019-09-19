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

##Análise de Outliers----

library(ggplot2)
library(cowplot)
library(plotly)
library(GGally)
library(tibble)
library(clValid)
library(lemon)

#Possíveis outliers:

df <- df %>% 
  tibble::rowid_to_column() %>% 
  dplyr::select(1:11) 

df1 <- df %>% 
  dplyr::filter(rowid %in% c("107","173","175","164","108")) %>% 
  dplyr::mutate(Out=1)

df2 <- df1 %>% 
  dplyr::right_join(df,by="rowid") %>% 
  dplyr::select(c(1,12:22)) %>% 
  dplyr::mutate(Out = ifelse(is.na(Out),0,1)) %>% 
  dplyr::select(1,3:12,2) %>% 
  dplyr::mutate(Out=as.factor(Out))

boxplot_1 <- ggplot(df2)+
  geom_boxplot(aes(y = Na.y,x = Tipo.y),
               color="black",fill="red") +
  geom_jitter(aes(y = Na.y,x = Tipo.y,shape=Out,col=rowid,size = Out),alpha=0.5)+
  xlab(label = "Tipo") +
  ylab(label="%")
boxplot_1

ggplotly(boxplot_1)

df3 <- df2 %>% 
  dplyr::filter(!(rowid %in% c("107","164","108"))) %>% 
  dplyr::select(2:10) %>% 
  base::scale()

cmin <- 2
cmax <- 10
algoritmos <- c("hierarchical","kmeans","pam")
intern <- clValid::clValid(df3, cmin:cmax, 
                           clMethods = algoritmos, validation = "internal")
summary(intern)   

plot(intern)

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

dend_ch <- factoextra::fviz_dend(vidro.Dend2, k = 2, cex = 0,
                                 k_colors = c("black", "black"),
                                 main  = "Dendrograma - Vidro",
                                 ylab  = "Distâncias", sub = "",
                                 xlab  = "Objetos",
                                 lwd   = 1) +
  # geom_hline(yintercept = 80, linetype = 2, size = 1) +
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

dend_ch

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

ggplotly(dend_ch2)
