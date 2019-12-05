---
title: "Modelos Multiclasse - Classificação de Vidros para Análise Forense"
author: "Brenner Silva e Marcello Pessoa"
date: "05/12/2019"
output:
  rmdformats::readthedown:
    self_contained: true
    thumbnails: true
    lightbox: true
    gallery: false
    highlight: tango
---

<style>
pre code, pre, code {
  white-space: pre !important;
  overflow-x: scroll !important;
  word-break: keep-all !important;
  word-wrap: initial !important;
}
</style>





<img src="https://i.imgur.com/xVOrqDf.png" width="100%" height="528" style="display: block; margin: auto;" />

## APRESENTAÇÃO

<p style="text-align: justify">

**Universidade Federal da Bahia - UFBA**
\
**[Escola Politécnica](http://www.eng.ufba.br/)**
\
**Programa de Pós-Graduação em Engenharia Industrial - [PEI/UFBA](http://www.pei.ufba.br/)**
\
**Departamento de Engenharia Química - DEQ**
\
**ENG436 - Tópicos Especiais em Engenharia**
\

\
O presente trabalho foi desenvolvido como parte de avaliação para a disciplina Tópicos Especiais em Engenharia, ministrada pela docente Karla Esquerre da Escola Politécnica da UFBA.
\

</p>

***

## DESENVOLVIMENTO

<p style="text-align: justify">

Vestígios deixados na cena de um crime podem ser de extrema importância para a resolução do mesmo. Utilizando dos conhecimentos que envolvem o aprendizado de máquina, é possível construir modelos preditivos que deem suporte as análises e obter importantes informações sobre padrões de banco de dados criminológicos. 
\
Diferentes estudos utilizam algoritmos de aprendizado de máquina para tarefas de classificação, procurando otimizar os modelos com variadas técnicas. No presente trabalho fora proposto um modelo preditivo de classificação de vidro multiclasse, utilizando do algoritmo de Random Forest para diferentes cenários de variáveis e técnicas de balanceamento de classes.
\

</p>


## DATASET E CODE {.tabset .tabset-fade}

<p style="text-align: justify">

O presente *dataset* em análise é um conjunto de objetos de identificação de vidro da UC Irvine Machine Learning Repository, disponível no [Kaggle](https://www.kaggle.com/uciml/glass), contendo 10 atributos, sendo a resposta o tipo do vidro (*label* / rótulo). 
\
Os atributos em análise são:
\

* 01- RI: Índice de Refração;

* 02- Na: Sódio (medição unitária: porcentagem em peso no óxido correspondente, como são os atributos 03-09);

* 03- Mg: Magnésio;

* 04- Al: Alumínio;

* 05- Si: Silício;

* 06- K: Potássio;

* 07- Ca: Cálcio;

* 08- Ba: Bário;

* 09- Fe: Ferro;

* 10- Tipo de Vidro;

  + X1 - Vidro *float* de construções;
  
  + X2 - Vidro "não" *float* de construções;
  
  + X3 - Vidro *float* de veículos;
  
  + X5 - Vidro de contêineres;
  
  + X6 - Utensílios de mesa (prato, xícara, etc);
  
  + X7 - [*headlamps*](https://www.google.com/search?q=headlamps&rlz=1C1ASUT_pt-BRBR683BR683&sxsrf=ACYBGNTmAvcoSNgIaLA95Js1S3mU0QBewQ:1575247410575&source=lnms&tbm=isch&sa=X&ved=2ahUKEwjsu9zy3ZXmAhXMo1kKHQItBEQQ_AUoAnoECA0QBA&biw=1366&bih=657).
  
\

\
Abaixo, iniciamos o código indicando o caminho da pasta de trabalho, carregando os pacotes demandados e o *dataset* a ser analisado. O uso do pacote `pacman` foi idealizado em facilitar a reprodutibilidade, dinamicidade de instalação e carregamento de pacotes. Especificamente o pacote `gg3D` demanda que o *download* seja realizado diretamente do **git**, e para tal, utilizamos o pacote `remotes`.
\

</p>



```r
setwd("D:/PEI/Doc/Disciplinas/Topicos/topicos_eng436")
# setwd("C:/Users/MARCELLO/Desktop/ENG436/topicos_eng436")

{
  if (!require("pacman")) install.packages("pacman")
  
  pacman::p_load(dplyr, readxl, ggplot2, GGally, lemon, scatterplot3d, skimr, 
                 plotly, knitr, tidyr, shiny, caret, randomForest, 
                 reshape2, multiROC, tibble, rmdformats, cowplot,
                 car, bestNormalize, forcats)
  
  if (!require("gg3D")) remotes::install_github("AckerDWM/gg3D")
  library(gg3D)
}

set.seed(42)
dataset <- readxl::read_excel("glass.xlsx")
```

<p style="text-align: justify">

O primeiro passo para qualquer estruturação ou análise, é o conhecimento acerca da estrutura inicial dos dados. Para tanto, utilizaremos a função `glimpse` do pacote `dplyr`.
\

</p>


```r
dplyr::glimpse(dataset)
```

```
Observations: 214
Variables: 10
$ RI   <dbl> 152101, 151761, 151618, 151766, 151742, 151596, 151743, 151756...
$ Na   <chr> "13.64", "13.89", "13.53", "13.21", "13.27", "12.79", "13.3", ...
$ Mg   <chr> "4.49", "3.6", "3.55", "3.69", "3.62", "3.61", "3.6", "3.61", ...
$ Al   <chr> "1.1", "1.36", "1.54", "1.29", "1.24", "1.62", "1.14", "1.05",...
$ Si   <chr> "71.78", "72.73", "72.99", "72.61", "73.08", "72.97", "73.09",...
$ K    <chr> "0.06", "0.48", "0.39", "0.57", "0.55", "0.64", "0.58", "0.57"...
$ Ca   <chr> "8.75", "7.83", "7.78", "8.22", "8.07", "8.07", "8.17", "8.24"...
$ Ba   <chr> "0", "0", "0", "0", "0", "0", "0", "0", "0", "0", "0", "0", "0...
$ Fe   <chr> "0", "0", "0", "0", "0", "0.26", "0", "0", "0", "0.11", "0.24"...
$ Type <dbl> 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1,...
```

<p style="text-align: justify">

Podemos observar que apenas a variável preditora **RI** está como *dbl* (*double*). Deste modo, transformaremos as *chr* (*character*) para números, tendo em vista a necessidade para exploração descritiva e construção de modelos. A variável *Type* é o *label* (rótulo) de nossas classes de vidro, e essas deverão passar a ser fatores, sendo renomeada para *Tipo*.
\
A variável *RI* será dividida por $10^5$ para adequação de escala real.
\

</p>


```r
df <- dataset %>% 
  dplyr::mutate_if(is.character, as.numeric) %>% 
  dplyr::mutate(Tipo = as.factor(Type),
                RI = RI/(10^5)) %>% 
  dplyr::select(-Type)

levels(df$Tipo) <- make.names(levels(factor(df$Tipo)))
dplyr::glimpse(df)
```

```
Observations: 214
Variables: 10
$ RI   <dbl> 1.52101, 1.51761, 1.51618, 1.51766, 1.51742, 1.51596, 1.51743,...
$ Na   <dbl> 13.64, 13.89, 13.53, 13.21, 13.27, 12.79, 13.30, 13.15, 14.04,...
$ Mg   <dbl> 4.49, 3.60, 3.55, 3.69, 3.62, 3.61, 3.60, 3.61, 3.58, 3.60, 3....
$ Al   <dbl> 1.10, 1.36, 1.54, 1.29, 1.24, 1.62, 1.14, 1.05, 1.37, 1.36, 1....
$ Si   <dbl> 71.78, 72.73, 72.99, 72.61, 73.08, 72.97, 73.09, 73.24, 72.08,...
$ K    <dbl> 0.06, 0.48, 0.39, 0.57, 0.55, 0.64, 0.58, 0.57, 0.56, 0.57, 0....
$ Ca   <dbl> 8.75, 7.83, 7.78, 8.22, 8.07, 8.07, 8.17, 8.24, 8.30, 8.40, 8....
$ Ba   <dbl> 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,...
$ Fe   <dbl> 0.00, 0.00, 0.00, 0.00, 0.00, 0.26, 0.00, 0.00, 0.00, 0.11, 0....
$ Tipo <fct> X1, X1, X1, X1, X1, X1, X1, X1, X1, X1, X1, X1, X1, X1, X1, X1...
```


<p style="text-align: justify">

Com o ajuste inicial do nosso `df`, foi modificado a classe das variáveis nos objetos  e ajustado os fatores / classes / labels / rótulos dos vidros, sendo estes renomeadas para ${X1,\;X2,\;X3,\;X5, X6\;e\;X7}$, para posterior uso na construção de modelo preditivo.
\
Agora vamos analisar se há `NA` ou `NaN` nos nossos dados, e, vamos aproveitar para calcular as estatísticas de tendência central e realizar de modo simultâneo um histograma de nossas variáveis de interesse.
\
Abaixo, utilizaremos as abas `Análise 1` e `Análise 2` para expor nossas informações descritas até aqui. Sendo que a aba `Análise 2`, servirá para nos aprofundarmos na análise descritiva com as variáveis de interesse de acordo com o label.
\

</p>

### Análise 1.1 


```r
# df %>% 
#   dplyr::select(-c("Tipo")) %>% 
#   skimr::skim() %>% 
#   dplyr::rename("Tipo da Variável" = skim_type, "Variável" = skim_variable, "nº NA" = n_missing, 
#                 "Média"  = numeric.mean, "DP" = numeric.sd,
#                 "p0" = numeric.p0, "p25" = numeric.p25, "p50" =	numeric.p50,
#                 "p75" = numeric.p75,"p100" = numeric.p100,
#                 "Histograma" =	numeric.hist) %>% 
#   dplyr::select(-c("complete_rate")) %>% 
#   knitr::kable()

# plotx <- df %>% 
#   dplyr::mutate_at(1:9, funs(scale)) %>% 
#   tidyr::gather("Var", "Valor",-Tipo) %>% 
#   ggplot2::ggplot() +
#   geom_jitter(aes(x = Var, y = Valor, col = Tipo), alpha = 0.6, width = 0.15) +
#   xlab("Variáveis") +
#   ylab("z-score") +
#   theme_bw() +
#   theme(legend.position = "bottom")
# plotx1 <- df %>% 
#   dplyr::mutate_at(1:9, funs(scale)) %>% 
#   tidyr::gather("Var", "Valor",-Tipo) %>% 
#   ggplot2::ggplot() +
#   geom_boxplot(aes(x = Var, y = Valor),
#                alpha = 0.9, show.legend = F, width = 0.4) +
#   xlab("Variáveis") +
#   ylab("z-score") +
#   theme_bw()
# cowplot::plot_grid(plotx, plotx1, align = "hv", axis = "bt")
```

### Análise 1.2


```r
# df %>% 
#   dplyr::group_by(Tipo) %>% 
#   skimr::skim() %>% 
#   dplyr::rename("Variável" = skim_variable, "Classe" = Tipo, 
#                 "Média"  = numeric.mean, "DP" = numeric.sd,
#                 "p0" = numeric.p0, "p25" = numeric.p25, "p50" =	numeric.p50,
#                 "p75" = numeric.p75,"p100" = numeric.p100,
#                 "Histograma" =	numeric.hist) %>% 
#   dplyr::select(-c("skim_type", "complete_rate", "n_missing")) %>% 
#   knitr::kable()
```

##

***

<p style="text-align: justify">

Como pode-se perceber não temos `NA` ou `NaN` no nosso `df`. Logo, não temos a necessidade de inputar dados faltantes.
\
Nossas variáveis não apresentam distribuições claramente semelhantes. Do ponto de vista descritivo, comparando classes numa mesma variável, a classe **X3** apresenta menor valor médio de **RI**, e **X5** e **X6** apresentam os maiores valores. E, de modo inverso, **X5** e **X6** apresentam os menores valores de desvio padrão (DP), e **X3** apresenta o maior valor.
\
Da variável **Na** os maiores valores de média e DP são referentes a **X6**, e para **X1** e **X3** os valores de média e DP estão próximos. Os valores de DP de **X1** e **X3** para **Mg**, estão muito abaixo que para as outras classes, e os valores de média estão próximos.
\
**X1** e **X2** apresentam baixos valores de DP em **Al**. Em **K**, **X6** apresenta média e DP iguais a zero, e **X5** apresenta elevado DP. Em **Ca**, as médias de **X1** e **X3** são muito próximas, e ambas as classes tem baixo valor de DP. E, para **Ba** e **Fe**, a média e o DP de **X6** são iguais a zero.
\
Contudo é interessante frisar a semelhança entre os dados das classes **X1** e **X3**, apresentando uma leve divergência apenas para **RI** e **Ba**.
\

</p>



```r
# plotx <- df %>% 
#   GGally::ggpairs(columns = 1:9, 
#                   mapping = aes(color = Tipo, alpha = 0.5),
#                   upper = list(continuous = wrap("cor", size = 2)),
#                   lower = list(continuous = wrap("points", alpha = 0.3)))
# shiny::div(plotly::ggplotly(plotx), align = "center")
# 
# plotx <- df %>% 
#   dplyr::filter(Tipo %in% c("X1", "X3")) %>% 
#   GGally::ggpairs(columns = 1:9, 
#                   mapping = aes(color = Tipo, alpha = 0.5),
#                   upper = list(continuous = wrap("cor", size = 2.5)),
#                   lower = list(continuous = wrap("points", alpha = 0.3)))
# shiny::div(plotly::ggplotly(plotx), align = "center")
```

<p style="text-align: justify">

Aparentemente não é possível visualizar um padrão claro entre as classes, e através da nossa matriz de dispersão, não conseguimos identificar situação de colinearidade entre as variáveis.
\
A homogeneidade entre os objetos pode, desde já, nos nortear quanto a dificuldade de construção de modelo preditivo com bons valores de sensibilidade e especificidade. E se focarmos nos grupos **X1** e **X3**, que é este, que desde a estatística básica apresenta homogeneidade, poderemos notar o quão similares os grupos são.
\

</p>

### Verificando Especificidades em X1 e X3 {.tabset .tabset-fade}

<p style="text-align: justify">

Os conjuntos de objetos pertencentes a **X1** e **X3** apresentam comportamento muito semelhante, e, em algum casos, com apresentando comportamento similar a uma situação bimodal, e para essas, realizaremos nosso `ggpairs` de formas separadas, dada as devidas condições anunciadas nas abas, e também utilizaremos o recurso de representação em 3D, a fim de tentar melhorar a perspectiva visual.
\

</p>

#### RI > 1 


```r
# plotx <- df %>% 
#   dplyr::filter(Tipo %in% c("X1", "X3"),
#                 RI > 1) %>% 
#   GGally::ggpairs(columns = 1:9, 
#                   mapping = aes(color = Tipo, alpha = 0.5),
#                   upper = list(continuous = wrap("cor", size = 2.5)),
#                   lower = list(continuous = wrap("points", alpha = 0.3)))
# shiny::div(plotly::ggplotly(plotx), align = "center")
```

#### RI < 1


```r
# plotx <- df %>% 
#   dplyr::filter(Tipo %in% c("X1", "X3"),
#                 RI < 1) %>% 
#   GGally::ggpairs(columns = 1:9, 
#                   mapping = aes(color = Tipo, alpha = 0.5),
#                   upper = list(continuous = wrap("cor", size = 2.5)),
#                   lower = list(continuous = wrap("points", alpha = 0.3)))
# shiny::div(plotly::ggplotly(plotx), align = "center")
```

#### K > 0.4


```r
# plotx <- df %>% 
#   dplyr::filter(Tipo %in% c("X1", "X3"),
#                 K > 0.4) %>% 
#   GGally::ggpairs(columns = 1:9, 
#                   mapping = aes(color = Tipo, alpha = 0.5),
#                   upper = list(continuous = wrap("cor", size = 2.5)),
#                   lower = list(continuous = wrap("points", alpha = 0.3)))
# shiny::div(plotly::ggplotly(plotx), align = "center")
```

#### K < 0.4


```r
# plotx <- df %>% 
#   dplyr::filter(Tipo %in% c("X1", "X3"),
#                 K < 0.4) %>% 
#   GGally::ggpairs(columns = 1:9, 
#                   mapping = aes(color = Tipo, alpha = 0.5),
#                   upper = list(continuous = wrap("cor", size = 2.5)),
#                   lower = list(continuous = wrap("points", alpha = 0.3)))
# shiny::div(plotly::ggplotly(plotx), align = "center")
```

#### 3D


```r
# plot1 <- df %>% 
#   dplyr::filter(Tipo %in% c("X1", "X3")) %>% 
#   ggplot(aes(x = Fe, y = RI, z = K, 
#              color = Tipo)) + 
#   theme_void() +
#   theme(plot.background = element_rect(color = "black")) +
#   axes_3D(theta = 0) +
#   stat_3D(theta = 0)
#   
# plot2 <- df %>% 
#   dplyr::filter(Tipo %in% c("X1", "X3")) %>% 
#   ggplot(aes(x = Fe, y = RI, z = K, 
#              color = Tipo)) + 
#   theme_void() +
#   theme(plot.background = element_rect(color = "black")) +
#   axes_3D(theta = 90) +
#   stat_3D(theta = 90)
# 
# plot3 <- df %>% 
#   dplyr::filter(Tipo %in% c("X1", "X3")) %>% 
#   ggplot(aes(x = Fe, y = RI, z = K, 
#              color = Tipo)) + 
#   theme_void() +
#   theme(plot.background = element_rect(color = "black")) +
#   axes_3D(theta = 180) +
#   stat_3D(theta = 180)
# 
# plot4 <- df %>% 
#   dplyr::filter(Tipo %in% c("X1", "X3")) %>% 
#   ggplot(aes(x = Fe, y = RI, z = K, 
#              color = Tipo)) + 
#   theme_void() +
#   theme(plot.background = element_rect(color = "black")) +
#   axes_3D(theta = 270) +
#   stat_3D(theta = 270)
# 
# cowplot::plot_grid(plot1, plot2, plot3, plot4, align = "hv",
#                    labels = c('0°', '90°', '180°', '270°'))
# ```
# 
# #### 3D interactive
# 
# ```{r, message=FALSE, warning=FALSE, cache = TRUE, fig.align='center'}
# cores <- c("royalblue1", "darkcyan", "green1", "black", "goldenrod3", "red2")
# 
# df %>% 
#   plotly::plot_ly(x = ~RI, y = ~Ca, z = ~Fe, color = ~Tipo, 
#                 colors = cores, opacity = 0.4, stroke = "black") %>%
#   add_markers() %>%
#   layout(scene = list(xaxis = list(title = 'RI'),
#                       yaxis = list(title = 'Ca'),
#                       zaxis = list(title = 'Fe')))
# df %>% 
#   plotly::plot_ly(x = ~Al, y = ~Mg, z = ~Fe, color = ~Tipo, 
#                 colors = cores, opacity = 0.4, stroke = "black") %>%
#   add_markers() %>%
#   layout(scene = list(xaxis = list(title = 'Al'),
#                       yaxis = list(title = 'Mg'),
#                       zaxis = list(title = 'Fe')))
```

#### 3D interactive para X1 e X3


```r
# df %>%
#   dplyr::filter(Tipo %in% c("X1", "X3")) %>%
#   plotly::plot_ly(x = ~RI, y = ~Ca, z = ~Fe, color = ~Tipo, 
#                 colors = cores, opacity = 0.4, stroke = "black") %>%
#   add_markers() %>%
#   layout(scene = list(xaxis = list(title = 'RI'),
#                       yaxis = list(title = 'Ca'),
#                       zaxis = list(title = 'Fe')))
# df %>% 
#   dplyr::filter(Tipo %in% c("X1", "X3")) %>%
#   plotly::plot_ly(x = ~Al, y = ~Mg, z = ~Fe, color = ~Tipo, 
#                 colors = cores, opacity = 0.4, stroke = "black") %>%
#   add_markers() %>%
#   layout(scene = list(xaxis = list(title = 'Al'),
#                       yaxis = list(title = 'Mg'),
#                       zaxis = list(title = 'Fe')))
```

###

***

<p style="text-align: justify">

Através da análise visual dos gráficos de dispersão é notável que alguns pontos mais distantes da massa de dados. E, apesar do problema em análise abordar variáveis com baixa incerteza de medição e elucidarem grandezas físico-química, realizaremos análise visual para discriminação de potenciais valores aberrantes (*outliers*).
\
Tal fato será importante, também, para tentar minimizar o possível *bias* gerado futuramento no modelo preditor, pois, é nítido que as classes (*labels*) estão desbalanceados, como pode ser visualizado abaixo, onde, de 214 objetos, apenas 4.21% representam a classe **X6**.
\
Contudo, a priori, procederemos com cautela pois **X3** e **X5** também apresentam baixos percentuais de representação no conjunto de objetos em análise, como pode ser visualizado no chunk abaixo. E, no próximo tópico, tentaremos refinar o nosso `df` identificando possíveis *outliers*.
\

</p>



```r
table(df$Tipo)
```

```

X1 X2 X3 X5 X6 X7 
70 76 17 13  9 29 
```

### Outliers

<p style="text-align: justify">

Para iniciar o processo de identificação de possíveis *outliers*, criaremos uma coluna de `ID` no nosso `df` para poder facilitar o processo de manipulação de dados. 
\
Novamente, com o auxílio do gráfico de dispersão, iniciaremos a prospecção dos possíveis objetos enquadrados como *outliers*. E, utilizaremos a ferramenta de gráfico interativo para poder identificar as informações de variáveis e de `ID`.
\

</p>



```r
df_out <- df %>% 
  tibble::rowid_to_column()

# plotx <- df_out %>% 
#   GGally::ggpairs(columns = 2:10, 
#                   mapping = aes(color = Tipo, alpha = 0.5, ),
#                   upper = list(continuous = wrap("cor", size = 2.5)),
#                   lower = list(continuous = wrap("points", alpha = 0.3)))
# shiny::div(plotly::ggplotly(plotx), align = "center")
```

<p style="text-align: justify">

Foi notado que os objetos $107,\;108,\;164,\;173\;e\;175$ estão muito afastados da massa central de dados. Como pode ser evidenciado abaixo. Sendo $107\;e\;108$ pertencentes a classe **X2** e $164,\;173\;e\;175$ a classe **X5**.
\
A fim de tornar ainda mais evidente a informação, apresentaremos os gráficos de dispersão em maior tamanho e em escala *z-score*.
\

</p>


```r
df_out2 <- df_out %>% 
  dplyr::mutate(rowid = as.factor(rowid),
                Out = ifelse(rowid == "107" | rowid == "108" | rowid == "164" |
                               rowid == "173" | rowid == "175", "out", "ok")) %>% 
  dplyr::mutate_at(vars(2:10), funs(scale)) %>% # scale()
  tidyr::gather(key = "Variaveis", value = "valor", 2:10)

# plotx <- df_out2 %>% 
#   dplyr::mutate(rowid = as.numeric(as.character(rowid))) %>% 
#   ggplot2::ggplot() +
#   ggplot2::geom_jitter(aes(x = Variaveis, y = valor, fill = Out, 
#                           shape = Out), 
#                        alpha = 0.5, size = 3,
#                        show.legend = F) +
#   facet_wrap(~Tipo, scales = "free") +
#   scale_shape_manual(values = c(20, 21))
# shiny::div(plotly::ggplotly(plotx), align = "center")
# 
# plotx <- df_out2 %>% 
#   dplyr::filter(Variaveis == "Na") %>% 
#   ggplot2::ggplot() +
#   stat_boxplot(aes(x = Tipo, y = valor),
#                geom = "errorbar", width = 0.5) +
#   geom_boxplot(aes(x = Tipo, y = valor),
#                color = "black", show.legend = F, outlier.shape = NA) +
#   geom_point(aes(x = Tipo, y = valor,
#                  shape = Out, fill = Out, alpha = Out), 
#              size = 5, width = 0.2, show.legend = F,
#              position = position_jitter(w = 0.1, h = 0)) +
#   scale_shape_manual(values = c(24, 21)) +
#   scale_alpha_manual(values = c(0.2, 0.7)) +
#   scale_y_continuous(limits = c(min(df_out2$valor), max(df_out2$valor))) +
#   xlab(label = "Tipo")
# shiny::div(plotly::ggplotly(plotx), align = "center")
```

<p style="text-align: justify">

Contudo, ao plotar os pontos e box-plot em função da variável **Na**, podemos notar que os objetos $173\;e\;175$ não apresentam comportamento incomum ou valor aberrante para o parâmetro, logo, manteremos como *outliers* apenas os objetos $107,\;108\;e\;164$.
\

</p>

## CONSTRUINDO MODELOS PREDITIVOS PARA CLASSIFICAÇÃO DE VIDRO {.tabset .tabset-fade}

<p style="text-align: justify">

No presente trabalho utilizaremos o algoritmo de *Random Forest*, através do pacote `caret`, para poder proceder com a classificação multiclasse dos vidros. Adotaremos um número padrão de árvores (`ntree = 1500`), e todos os modelos gerados serão "cross-validados" através da técnica *k-fold*, onde $k=10$. Como nesse problema existe desbalanceamento de classes, utilizaremos também técnicas de compensação como *undersampling* e *oversampling*.
\
Deste modo, utilizando o conjunto de dados sem promover modificações (apenas removendo *outliers*), construiremos 03 modelos de classificação: 
\

* Modelo com dados originais;

* Modelo com *under*;

* Modelo com *over*.

\
Logo, para proceder com a construção dos nossos modelos de classificação, manipularemos os dados para removermos os *outliers*, separaremos nosso banco de dados em treino (`train_data`) e teste (`test_data`), na proporção de 70-30%, e criaremos o objeto `control_Cv` para poder proceder com a *cross-validation*.
\

</p>


```r
df_pred <- df_out %>% 
  dplyr::filter(!rowid %in% c("107","108","164")) %>% 
  dplyr::select(-rowid)

{
  set.seed(42)
  index <- caret::createDataPartition(df_pred$Tipo, p = 0.7, list = FALSE) # particao
  train_data <- df_pred[index, ]
  test_data  <- df_pred[-index, ]

  control_Cv_orig <- caret::trainControl(method  = "cv", # cross-validation
                                         number  = 10,
                                         classProbs = T,
                                         savePredictions = T,
                                         verboseIter = FALSE)
  
  control_Cv_up <- caret::trainControl(method  = "cv", # cross-validation
                                       number  = 10,
                                       classProbs = T,
                                       savePredictions = T,
                                       verboseIter = FALSE,
                                       sampling = "up")
  
  control_Cv_down <- caret::trainControl(method  = "cv", # cross-validation
                                         number  = 10,
                                         classProbs = T,
                                         savePredictions = T,
                                         verboseIter = FALSE,
                                         sampling = "down")
  }

arvores <- 1500
```



<p style="text-align: justify">
\
Com o `df` devidamente ajustado, partiremos então para a construção dos modelos:

\

### MODELO RF SEM REAMOSTRAGEM {.tabset .tabset-fade}

* 1. 1) *Random Forest* sem reamostragem (original)

\
</p>


```r
set.seed(42)
model_rf_m1 <- caret::train(Tipo ~ .,
                            data = train_data,
                            method = "rf",
                            ntree = arvores,
                            preProcess = c("scale", "center"),
                            trControl = control_Cv_orig)

cm_original_m1 <- caret::confusionMatrix(predict(model_rf_m1, test_data),                       
                                         test_data$Tipo)
cm_original_m1
```

```
Confusion Matrix and Statistics

          Reference
Prediction X1 X2 X3 X5 X6 X7
        X1 17  4  4  0  0  1
        X2  3 18  0  0  0  0
        X3  1  0  1  0  0  0
        X5  0  0  0  3  0  0
        X6  0  0  0  0  2  2
        X7  0  0  0  0  0  5

Overall Statistics
                                          
               Accuracy : 0.7541          
                 95% CI : (0.6271, 0.8554)
    No Information Rate : 0.3607          
    P-Value [Acc > NIR] : 4.418e-10       
                                          
                  Kappa : 0.6542          
                                          
 Mcnemar's Test P-Value : NA              

Statistics by Class:

                     Class: X1 Class: X2 Class: X3 Class: X5 Class: X6
Sensitivity             0.8095    0.8182   0.20000   1.00000   1.00000
Specificity             0.7750    0.9231   0.98214   1.00000   0.96610
Pos Pred Value          0.6538    0.8571   0.50000   1.00000   0.50000
Neg Pred Value          0.8857    0.9000   0.93220   1.00000   1.00000
Prevalence              0.3443    0.3607   0.08197   0.04918   0.03279
Detection Rate          0.2787    0.2951   0.01639   0.04918   0.03279
Detection Prevalence    0.4262    0.3443   0.03279   0.04918   0.06557
Balanced Accuracy       0.7923    0.8706   0.59107   1.00000   0.98305
                     Class: X7
Sensitivity            0.62500
Specificity            1.00000
Pos Pred Value         1.00000
Neg Pred Value         0.94643
Prevalence             0.13115
Detection Rate         0.08197
Detection Prevalence   0.08197
Balanced Accuracy      0.81250
```

### MODELO RF COM OVERSAMPLING {.tabset .tabset-fade}

<p style="text-align: justify">
\

* 1. 2) *Random Forest* com *oversampling*

\

</p>


```r
set.seed(42)
model_rf_up_m1 <- caret::train(Tipo ~ .,
                               data = train_data,
                               method = "rf",
                               ntree = arvores,
                               preProcess = c("scale", "center"),
                               trControl = control_Cv_up)

cm_over_m1 <- caret::confusionMatrix(predict(model_rf_up_m1, test_data),
                                     test_data$Tipo)
cm_over_m1
```

```
Confusion Matrix and Statistics

          Reference
Prediction X1 X2 X3 X5 X6 X7
        X1 18  4  3  0  0  1
        X2  3 18  0  0  0  1
        X3  0  0  2  0  0  0
        X5  0  0  0  3  0  0
        X6  0  0  0  0  2  0
        X7  0  0  0  0  0  6

Overall Statistics
                                         
               Accuracy : 0.8033         
                 95% CI : (0.6816, 0.894)
    No Information Rate : 0.3607         
    P-Value [Acc > NIR] : 1.86e-12       
                                         
                  Kappa : 0.7206         
                                         
 Mcnemar's Test P-Value : NA             

Statistics by Class:

                     Class: X1 Class: X2 Class: X3 Class: X5 Class: X6
Sensitivity             0.8571    0.8182   0.40000   1.00000   1.00000
Specificity             0.8000    0.8974   1.00000   1.00000   1.00000
Pos Pred Value          0.6923    0.8182   1.00000   1.00000   1.00000
Neg Pred Value          0.9143    0.8974   0.94915   1.00000   1.00000
Prevalence              0.3443    0.3607   0.08197   0.04918   0.03279
Detection Rate          0.2951    0.2951   0.03279   0.04918   0.03279
Detection Prevalence    0.4262    0.3607   0.03279   0.04918   0.03279
Balanced Accuracy       0.8286    0.8578   0.70000   1.00000   1.00000
                     Class: X7
Sensitivity            0.75000
Specificity            1.00000
Pos Pred Value         1.00000
Neg Pred Value         0.96364
Prevalence             0.13115
Detection Rate         0.09836
Detection Prevalence   0.09836
Balanced Accuracy      0.87500
```

### MODELO RF COM UNDERSAMPLING {.tabset .tabset-fade}

<p style="text-align: justify">
\

* 1. 3) *Random Forest* com *undersampling*

\

</p>


```r
set.seed(42)
model_rf_down_m1 <- caret::train(Tipo ~ .,
                                 data = train_data,
                                 method = "rf",
                                 ntree = arvores,
                                 preProcess = c("scale", "center"),
                                 trControl = control_Cv_down)

cm_down_m1 <- caret::confusionMatrix(predict(model_rf_down_m1, test_data), 
                                     test_data$Tipo)
cm_down_m1
```

```
Confusion Matrix and Statistics

          Reference
Prediction X1 X2 X3 X5 X6 X7
        X1 13  4  1  0  0  0
        X2  4 12  2  0  0  1
        X3  4  4  2  0  0  0
        X5  0  1  0  3  0  1
        X6  0  0  0  0  2  0
        X7  0  1  0  0  0  6

Overall Statistics
                                          
               Accuracy : 0.623           
                 95% CI : (0.4896, 0.7439)
    No Information Rate : 0.3607          
    P-Value [Acc > NIR] : 2.787e-05       
                                          
                  Kappa : 0.4989          
                                          
 Mcnemar's Test P-Value : NA              

Statistics by Class:

                     Class: X1 Class: X2 Class: X3 Class: X5 Class: X6
Sensitivity             0.6190    0.5455   0.40000   1.00000   1.00000
Specificity             0.8750    0.8205   0.85714   0.96552   1.00000
Pos Pred Value          0.7222    0.6316   0.20000   0.60000   1.00000
Neg Pred Value          0.8140    0.7619   0.94118   1.00000   1.00000
Prevalence              0.3443    0.3607   0.08197   0.04918   0.03279
Detection Rate          0.2131    0.1967   0.03279   0.04918   0.03279
Detection Prevalence    0.2951    0.3115   0.16393   0.08197   0.03279
Balanced Accuracy       0.7470    0.6830   0.62857   0.98276   1.00000
                     Class: X7
Sensitivity            0.75000
Specificity            0.98113
Pos Pred Value         0.85714
Neg Pred Value         0.96296
Prevalence             0.13115
Detection Rate         0.09836
Detection Prevalence   0.11475
Balanced Accuracy      0.86557
```

##



## TRANSFORMANDO DADOS

<p style="text-align: justify">

Apesar da robustez do algoritmo de `RF` e dos resultados satisfatórios removendo alguns objetos considerados como aberrantes, é notório que as distribuições de nossas variáveis preditoras **K**, **Mg** e **RI** apresentam elevada assimetria, e isto pode impactar negativamente o desempenho do modelo proposto.
\
Deste modo, iremos proceder com manutenção dos objetos considerados inicialmente como *outliers* e realizaremos a transformação das variáveis supracitadas através da técnica de transoformação de Yeo–Johnson. Tal fato se dá pela homogeneização da escolha do método de transformação pois algumas de nosss variáveis apresentam valor igual a 0 (zero), logo não poderemos utiliar técnica como transformação Box-Cox para todas as variáveis.
\
Abaixo, podemos visualizar que as variáveis mencionadas não apresentam visualmente padrão de distribuição normal.
\
</p>


```r
shiny::div(plotly::ggplotly(df %>% 
                              dplyr::select(RI, K, Mg, Tipo) %>% 
                              tidyr::gather(key = "Variavel", value = "valor", c(RI, K, Mg)) %>% 
                              ggplot2::ggplot() +
                              geom_density(aes(valor, fill = Tipo), alpha = 0.5) +
                              facet_wrap(~Variavel, scales = "free") +
                              ylab("Densidade") +
                              theme_minimal_hgrid(12), 
                            align = "center"))
```

preservedfa9f90f1580cf03

```r
shiny::div(plotly::ggplotly(df %>% 
                              dplyr::select(RI, K, Mg) %>% 
                              tidyr::gather(key = "Variavel", value = "valor") %>% 
                              ggplot2::ggplot() +
                              geom_density(aes(valor, fill = Variavel), alpha = 0.5) +
                              facet_wrap(~Variavel, scales = "free") +
                              ylab("Densidade") +
                              theme_minimal_hgrid(12), 
                            align = "center"))
```

preserve3953aa0220c8753a

## CONSTRUINDO NOVOS MODELOS {.tabset .tabset-fade}

<p style="text-align: justify">
Apesar dos bons valores de Acurácia, a classe **X3** não apresenta bom *score* de Verdaadeiros Positivos, logo, novos modelos serão propostos para cenários similares, tendo em vista o melhor ajuste para o modelo preditor.
\
</p>

### Cenário A

<p style="text-align: justify">
Para construir os novos modelos, precisaremos reajustar nosso `df`.
\
</p>


```r
set.seed(42)
BN_RI <- bestNormalize::bestNormalize(df$RI, allow_lambert_s = TRUE)
```

```r
BN_K  <- bestNormalize::bestNormalize(df$K,  allow_lambert_s = TRUE)
```

```r
BN_Mg <- bestNormalize::bestNormalize(df$Mg, allow_lambert_s = TRUE)
```

```r
df_pred2 <- df %>% 
  dplyr::mutate(RI_tr = car::yjPower(df$RI, BN_RI$other_transforms$yeojohnson$lambda),
                K_tr  = car::yjPower(df$RI,  BN_K$other_transforms$yeojohnson$lambda),
                Mg_tr = car::yjPower(df$RI, BN_Mg$other_transforms$yeojohnson$lambda)) %>% 
  dplyr::select(-c(RI, K, Mg))
```

<p style="text-align: justify">
Para poder incrementar performance aos modelos propostos criaremos uma relação não linear entre os preditores e a resposta executando a regressão usando transformações dos preditores [(James et al., 2013)](https://www.ime.unicamp.br/~dias/Intoduction%20to%20Statistical%20Learning.pdf#page=160), onde iremos incluir variáveis $X^2$ no nosso modelo.
\
E, para implementar tal estratéfia, as variáveis **Fe** e **Ca** serão elevadas a potência 2, baseado na análise exploratória e o **feedback** dos modelos construídos no moemnto anterior.
\
Poderiamos pensar em algo similar para a variável **Ba**, contudo, a explicação para semelhança de medidas de tendência central em **X1** e **X3** para **Ba** se dá pela elevada quantidade de objetos com valor 0 (zero) para esta variável preditora.
\
</p>


```r
df_pred2 <- df_pred2 %>% 
  dplyr::mutate(Fe_2 = Fe^2,
                Ca_2 = Ca^2)
```

<p style="text-align: justify">
Nossos novos modelos propostos terão configuração análoga ao primeiros modelos, com dados "originais", *undersampling* e *oversampling*.
\
</p>


```r
{
  set.seed(42)
  index <- caret::createDataPartition(df_pred2$Tipo, p = 0.7, list = FALSE) # particao
  train_data <- df_pred2[index, ]
  test_data  <- df_pred2[-index, ]
  }
```

* 2. 1. A) *Random Forest* sem reamostragem (original)

\
</p>


```r
set.seed(42)
model_rf_m2aori <- caret::train(Tipo ~ .,
                                data = train_data,
                                method = "rf",
                                ntree = arvores,
                                preProcess = c("scale", "center"),
                                trControl = control_Cv_orig)
                                               
cm_original_m2aori <- caret::confusionMatrix(predict(model_rf_m2aori, test_data),
                                             test_data$Tipo)
cm_original_m2aori
```

```
Confusion Matrix and Statistics

          Reference
Prediction X1 X2 X3 X5 X6 X7
        X1 16  1  1  0  1  2
        X2  5 20  3  1  0  0
        X3  0  0  1  0  0  0
        X5  0  1  0  2  0  0
        X6  0  0  0  0  1  1
        X7  0  0  0  0  0  5

Overall Statistics
                                         
               Accuracy : 0.7377         
                 95% CI : (0.6093, 0.842)
    No Information Rate : 0.3607         
    P-Value [Acc > NIR] : 2.295e-09      
                                         
                  Kappa : 0.6223         
                                         
 Mcnemar's Test P-Value : NA             

Statistics by Class:

                     Class: X1 Class: X2 Class: X3 Class: X5 Class: X6
Sensitivity             0.7619    0.9091   0.20000   0.66667   0.50000
Specificity             0.8750    0.7692   1.00000   0.98276   0.98305
Pos Pred Value          0.7619    0.6897   1.00000   0.66667   0.50000
Neg Pred Value          0.8750    0.9375   0.93333   0.98276   0.98305
Prevalence              0.3443    0.3607   0.08197   0.04918   0.03279
Detection Rate          0.2623    0.3279   0.01639   0.03279   0.01639
Detection Prevalence    0.3443    0.4754   0.01639   0.04918   0.03279
Balanced Accuracy       0.8185    0.8392   0.60000   0.82471   0.74153
                     Class: X7
Sensitivity            0.62500
Specificity            1.00000
Pos Pred Value         1.00000
Neg Pred Value         0.94643
Prevalence             0.13115
Detection Rate         0.08197
Detection Prevalence   0.08197
Balanced Accuracy      0.81250
```


<p style="text-align: justify">
\

* 2. 2. A) *Random Forest* com *oversampling*

\

</p>


```r
set.seed(42)
model_rf_up_m2aup <- caret::train(Tipo ~ .,
                                  data = train_data,
                                  method = "rf",
                                  ntree = arvores,
                                  preProcess = c("scale", "center"),
                                  trControl = control_Cv_up)

cm_over_m2aup <- caret::confusionMatrix(predict(model_rf_up_m2aup, test_data),
                                        test_data$Tipo)
cm_over_m2aup
```

```
Confusion Matrix and Statistics

          Reference
Prediction X1 X2 X3 X5 X6 X7
        X1 16  1  2  0  1  3
        X2  5 19  2  1  0  0
        X3  0  0  1  0  0  0
        X5  0  0  0  2  0  0
        X6  0  2  0  0  1  0
        X7  0  0  0  0  0  5

Overall Statistics
                                          
               Accuracy : 0.7213          
                 95% CI : (0.5917, 0.8285)
    No Information Rate : 0.3607          
    P-Value [Acc > NIR] : 1.099e-08       
                                          
                  Kappa : 0.5991          
                                          
 Mcnemar's Test P-Value : NA              

Statistics by Class:

                     Class: X1 Class: X2 Class: X3 Class: X5 Class: X6
Sensitivity             0.7619    0.8636   0.20000   0.66667   0.50000
Specificity             0.8250    0.7949   1.00000   1.00000   0.96610
Pos Pred Value          0.6957    0.7037   1.00000   1.00000   0.33333
Neg Pred Value          0.8684    0.9118   0.93333   0.98305   0.98276
Prevalence              0.3443    0.3607   0.08197   0.04918   0.03279
Detection Rate          0.2623    0.3115   0.01639   0.03279   0.01639
Detection Prevalence    0.3770    0.4426   0.01639   0.03279   0.04918
Balanced Accuracy       0.7935    0.8293   0.60000   0.83333   0.73305
                     Class: X7
Sensitivity            0.62500
Specificity            1.00000
Pos Pred Value         1.00000
Neg Pred Value         0.94643
Prevalence             0.13115
Detection Rate         0.08197
Detection Prevalence   0.08197
Balanced Accuracy      0.81250
```

<p style="text-align: justify">
\

* 2. 3. A) *Random Forest* com *undersampling*

\

</p>


```r
set.seed(42)
model_rf_down_m2aunder <- caret::train(Tipo ~ .,
                                       data = train_data,
                                       method = "rf",
                                       ntree = arvores,
                                       preProcess = c("scale", "center"),
                                       trControl = control_Cv_down)

cm_down_m2aunder <- caret::confusionMatrix(predict(model_rf_down_m2aunder, test_data), 
                                           test_data$Tipo)
cm_down_m2aunder
```

```
Confusion Matrix and Statistics

          Reference
Prediction X1 X2 X3 X5 X6 X7
        X1 15  4  0  0  1  1
        X2  3 13  3  0  0  0
        X3  3  2  2  0  0  1
        X5  0  3  0  3  0  0
        X6  0  0  0  0  1  1
        X7  0  0  0  0  0  5

Overall Statistics
                                          
               Accuracy : 0.6393          
                 95% CI : (0.5063, 0.7584)
    No Information Rate : 0.3607          
    P-Value [Acc > NIR] : 8.991e-06       
                                          
                  Kappa : 0.5138          
                                          
 Mcnemar's Test P-Value : NA              

Statistics by Class:

                     Class: X1 Class: X2 Class: X3 Class: X5 Class: X6
Sensitivity             0.7143    0.5909   0.40000   1.00000   0.50000
Specificity             0.8500    0.8462   0.89286   0.94828   0.98305
Pos Pred Value          0.7143    0.6842   0.25000   0.50000   0.50000
Neg Pred Value          0.8500    0.7857   0.94340   1.00000   0.98305
Prevalence              0.3443    0.3607   0.08197   0.04918   0.03279
Detection Rate          0.2459    0.2131   0.03279   0.04918   0.01639
Detection Prevalence    0.3443    0.3115   0.13115   0.09836   0.03279
Balanced Accuracy       0.7821    0.7185   0.64643   0.97414   0.74153
                     Class: X7
Sensitivity            0.62500
Specificity            1.00000
Pos Pred Value         1.00000
Neg Pred Value         0.94643
Prevalence             0.13115
Detection Rate         0.08197
Detection Prevalence   0.08197
Balanced Accuracy      0.81250
```


### Cenário B

<p style="text-align: justify">
De maneira similar ao Cenário A, das "novas" variáveis, manteremos apenas a abordagem considerando as variáveis transformadas atráves do método Yeo-Johnson.
\
</p>


```r
df_pred2 <- df %>% 
  dplyr::mutate(RI_tr = car::yjPower(df$RI, BN_RI$other_transforms$yeojohnson$lambda),
                K_tr  = car::yjPower(df$RI,  BN_K$other_transforms$yeojohnson$lambda),
                Mg_tr = car::yjPower(df$RI, BN_Mg$other_transforms$yeojohnson$lambda)) %>% 
  dplyr::select(-c(RI, K, Mg))

{
  set.seed(42)
  index <- caret::createDataPartition(df_pred2$Tipo, p = 0.7, list = FALSE) # particao
  train_data <- df_pred2[index, ]
  test_data  <- df_pred2[-index, ]
  }
```

* 2. 1. B) *Random Forest* sem reamostragem (original)

\
</p>


```r
set.seed(42)
model_rf_m2bori <- caret::train(Tipo ~ .,
                                data = train_data,
                                method = "rf",
                                tree = arvores,
                                preProcess = c("scale", "center"),
                                trControl = control_Cv_orig)
                                               
cm_original_m2bori <- caret::confusionMatrix(predict(model_rf_m2bori, test_data),
                                             test_data$Tipo)
cm_original_m2bori
```

```
Confusion Matrix and Statistics

          Reference
Prediction X1 X2 X3 X5 X6 X7
        X1 18  2  1  0  1  3
        X2  3 19  3  2  0  0
        X3  0  0  1  0  0  0
        X5  0  0  0  1  0  0
        X6  0  1  0  0  1  0
        X7  0  0  0  0  0  5

Overall Statistics
                                         
               Accuracy : 0.7377         
                 95% CI : (0.6093, 0.842)
    No Information Rate : 0.3607         
    P-Value [Acc > NIR] : 2.295e-09      
                                         
                  Kappa : 0.6173         
                                         
 Mcnemar's Test P-Value : NA             

Statistics by Class:

                     Class: X1 Class: X2 Class: X3 Class: X5 Class: X6
Sensitivity             0.8571    0.8636   0.20000   0.33333   0.50000
Specificity             0.8250    0.7949   1.00000   1.00000   0.98305
Pos Pred Value          0.7200    0.7037   1.00000   1.00000   0.50000
Neg Pred Value          0.9167    0.9118   0.93333   0.96667   0.98305
Prevalence              0.3443    0.3607   0.08197   0.04918   0.03279
Detection Rate          0.2951    0.3115   0.01639   0.01639   0.01639
Detection Prevalence    0.4098    0.4426   0.01639   0.01639   0.03279
Balanced Accuracy       0.8411    0.8293   0.60000   0.66667   0.74153
                     Class: X7
Sensitivity            0.62500
Specificity            1.00000
Pos Pred Value         1.00000
Neg Pred Value         0.94643
Prevalence             0.13115
Detection Rate         0.08197
Detection Prevalence   0.08197
Balanced Accuracy      0.81250
```


<p style="text-align: justify">
\

* 2. 2. B) *Random Forest* com *oversampling*

\

</p>


```r
set.seed(42)
model_rf_up_m2bup <- caret::train(Tipo ~ .,
                                  data = train_data,
                                  method = "rf",
                                  ntree = arvores,
                                  preProcess = c("scale", "center"),
                                  trControl = control_Cv_up)

cm_over_m2bup <- caret::confusionMatrix(predict(model_rf_up_m2bup, test_data),
                                        test_data$Tipo)
cm_over_m2bup
```

```
Confusion Matrix and Statistics

          Reference
Prediction X1 X2 X3 X5 X6 X7
        X1 18  3  1  0  1  2
        X2  3 17  4  1  0  0
        X3  0  0  0  0  0  0
        X5  0  0  0  2  0  1
        X6  0  2  0  0  1  0
        X7  0  0  0  0  0  5

Overall Statistics
                                          
               Accuracy : 0.7049          
                 95% CI : (0.5743, 0.8148)
    No Information Rate : 0.3607          
    P-Value [Acc > NIR] : 4.868e-08       
                                          
                  Kappa : 0.5762          
                                          
 Mcnemar's Test P-Value : NA              

Statistics by Class:

                     Class: X1 Class: X2 Class: X3 Class: X5 Class: X6
Sensitivity             0.8571    0.7727   0.00000   0.66667   0.50000
Specificity             0.8250    0.7949   1.00000   0.98276   0.96610
Pos Pred Value          0.7200    0.6800       NaN   0.66667   0.33333
Neg Pred Value          0.9167    0.8611   0.91803   0.98276   0.98276
Prevalence              0.3443    0.3607   0.08197   0.04918   0.03279
Detection Rate          0.2951    0.2787   0.00000   0.03279   0.01639
Detection Prevalence    0.4098    0.4098   0.00000   0.04918   0.04918
Balanced Accuracy       0.8411    0.7838   0.50000   0.82471   0.73305
                     Class: X7
Sensitivity            0.62500
Specificity            1.00000
Pos Pred Value         1.00000
Neg Pred Value         0.94643
Prevalence             0.13115
Detection Rate         0.08197
Detection Prevalence   0.08197
Balanced Accuracy      0.81250
```

<p style="text-align: justify">
\

* 2. 3. B) *Random Forest* com *undersampling*

\

</p>


```r
set.seed(42)
model_rf_down_m2bunder <- caret::train(Tipo ~ .,
                                       data = train_data,
                                       method = "rf",
                                       ntree = arvores,
                                       preProcess = c("scale", "center"),
                                       trControl = control_Cv_down)

cm_down_m2bunder <- caret::confusionMatrix(predict(model_rf_down_m2bunder, test_data), 
                                           test_data$Tipo)
cm_down_m2bunder
```

```
Confusion Matrix and Statistics

          Reference
Prediction X1 X2 X3 X5 X6 X7
        X1 15  2  0  1  1  1
        X2  3 15  4  0  0  0
        X3  3  2  1  0  0  1
        X5  0  1  0  1  0  0
        X6  0  2  0  0  1  1
        X7  0  0  0  1  0  5

Overall Statistics
                                          
               Accuracy : 0.623           
                 95% CI : (0.4896, 0.7439)
    No Information Rate : 0.3607          
    P-Value [Acc > NIR] : 2.787e-05       
                                          
                  Kappa : 0.4842          
                                          
 Mcnemar's Test P-Value : NA              

Statistics by Class:

                     Class: X1 Class: X2 Class: X3 Class: X5 Class: X6
Sensitivity             0.7143    0.6818   0.20000   0.33333   0.50000
Specificity             0.8750    0.8205   0.89286   0.98276   0.94915
Pos Pred Value          0.7500    0.6818   0.14286   0.50000   0.25000
Neg Pred Value          0.8537    0.8205   0.92593   0.96610   0.98246
Prevalence              0.3443    0.3607   0.08197   0.04918   0.03279
Detection Rate          0.2459    0.2459   0.01639   0.01639   0.01639
Detection Prevalence    0.3279    0.3607   0.11475   0.03279   0.06557
Balanced Accuracy       0.7946    0.7512   0.54643   0.65805   0.72458
                     Class: X7
Sensitivity            0.62500
Specificity            0.98113
Pos Pred Value         0.83333
Neg Pred Value         0.94545
Prevalence             0.13115
Detection Rate         0.08197
Detection Prevalence   0.09836
Balanced Accuracy      0.80307
```

### Cenário C

<p style="text-align: justify">
De maneira similar ao Cenário A, das "novas" variáveis, manteremos apenas a abordagem considerando as variáveis transformadas como $X^2$.
\
</p>



```r
df_pred2 <- df %>% 
  dplyr::mutate(Fe_2 = Fe^2,
                Ca_2 = Ca^2)
{
  set.seed(42)
  index <- caret::createDataPartition(df_pred2$Tipo, p = 0.7, list = FALSE) # particao
  train_data <- df_pred2[index, ]
  test_data  <- df_pred2[-index, ]
  }
```

* 2. 1. C) *Random Forest* sem reamostragem (original)

\
</p>


```r
set.seed(42)
model_rf_m2cori <- caret::train(Tipo ~ .,
                                data = train_data,
                                method = "rf",
                                ntree = arvores,
                                preProcess = c("scale", "center"),
                                trControl = control_Cv_orig)
                                               
cm_original_m2cori <- caret::confusionMatrix(predict(model_rf_m2cori, test_data),
                                             test_data$Tipo)
cm_original_m2cori
```

```
Confusion Matrix and Statistics

          Reference
Prediction X1 X2 X3 X5 X6 X7
        X1 17  4  4  0  0  2
        X2  4 18  0  1  1  0
        X3  0  0  1  0  0  0
        X5  0  0  0  2  0  0
        X6  0  0  0  0  1  1
        X7  0  0  0  0  0  5

Overall Statistics
                                          
               Accuracy : 0.7213          
                 95% CI : (0.5917, 0.8285)
    No Information Rate : 0.3607          
    P-Value [Acc > NIR] : 1.099e-08       
                                          
                  Kappa : 0.5967          
                                          
 Mcnemar's Test P-Value : NA              

Statistics by Class:

                     Class: X1 Class: X2 Class: X3 Class: X5 Class: X6
Sensitivity             0.8095    0.8182   0.20000   0.66667   0.50000
Specificity             0.7500    0.8462   1.00000   1.00000   0.98305
Pos Pred Value          0.6296    0.7500   1.00000   1.00000   0.50000
Neg Pred Value          0.8824    0.8919   0.93333   0.98305   0.98305
Prevalence              0.3443    0.3607   0.08197   0.04918   0.03279
Detection Rate          0.2787    0.2951   0.01639   0.03279   0.01639
Detection Prevalence    0.4426    0.3934   0.01639   0.03279   0.03279
Balanced Accuracy       0.7798    0.8322   0.60000   0.83333   0.74153
                     Class: X7
Sensitivity            0.62500
Specificity            1.00000
Pos Pred Value         1.00000
Neg Pred Value         0.94643
Prevalence             0.13115
Detection Rate         0.08197
Detection Prevalence   0.08197
Balanced Accuracy      0.81250
```


<p style="text-align: justify">
\

* 2. 2. C) *Random Forest* com *oversampling*

\

</p>


```r
set.seed(42)
model_rf_up_m2cup <- caret::train(Tipo ~ .,
                                  data = train_data,
                                  method = "rf",
                                  ntree = arvores,
                                  preProcess = c("scale", "center"),
                                  trControl = control_Cv_up)

cm_over_m2cup <- caret::confusionMatrix(predict(model_rf_up_m2cup, test_data),
                                    test_data$Tipo)
cm_over_m2cup
```

```
Confusion Matrix and Statistics

          Reference
Prediction X1 X2 X3 X5 X6 X7
        X1 18  3  5  0  0  1
        X2  3 17  0  0  0  0
        X3  0  1  0  0  0  0
        X5  0  0  0  3  0  1
        X6  0  0  0  0  2  0
        X7  0  1  0  0  0  6

Overall Statistics
                                          
               Accuracy : 0.7541          
                 95% CI : (0.6271, 0.8554)
    No Information Rate : 0.3607          
    P-Value [Acc > NIR] : 4.418e-10       
                                          
                  Kappa : 0.653           
                                          
 Mcnemar's Test P-Value : NA              

Statistics by Class:

                     Class: X1 Class: X2 Class: X3 Class: X5 Class: X6
Sensitivity             0.8571    0.7727   0.00000   1.00000   1.00000
Specificity             0.7750    0.9231   0.98214   0.98276   1.00000
Pos Pred Value          0.6667    0.8500   0.00000   0.75000   1.00000
Neg Pred Value          0.9118    0.8780   0.91667   1.00000   1.00000
Prevalence              0.3443    0.3607   0.08197   0.04918   0.03279
Detection Rate          0.2951    0.2787   0.00000   0.04918   0.03279
Detection Prevalence    0.4426    0.3279   0.01639   0.06557   0.03279
Balanced Accuracy       0.8161    0.8479   0.49107   0.99138   1.00000
                     Class: X7
Sensitivity            0.75000
Specificity            0.98113
Pos Pred Value         0.85714
Neg Pred Value         0.96296
Prevalence             0.13115
Detection Rate         0.09836
Detection Prevalence   0.11475
Balanced Accuracy      0.86557
```

<p style="text-align: justify">
\

* 2. 3. C) *Random Forest* com *undersampling*

\

</p>


```r
set.seed(42)
model_rf_down_m2cunder <- caret::train(Tipo ~ .,
                                       data = train_data,
                                       method = "rf",
                                       ntree = arvores,
                                       preProcess = c("scale", "center"),
                                       trControl = control_Cv_down)

cm_down_m2cunder <- caret::confusionMatrix(predict(model_rf_down_m2cunder, test_data), 
                                           test_data$Tipo)
cm_down_m2cunder
```

```
Confusion Matrix and Statistics

          Reference
Prediction X1 X2 X3 X5 X6 X7
        X1 14  5  1  0  0  1
        X2  2 12  2  0  0  0
        X3  5  2  2  0  0  0
        X5  0  2  0  3  0  0
        X6  0  0  0  0  2  1
        X7  0  1  0  0  0  6

Overall Statistics
                                          
               Accuracy : 0.6393          
                 95% CI : (0.5063, 0.7584)
    No Information Rate : 0.3607          
    P-Value [Acc > NIR] : 8.991e-06       
                                          
                  Kappa : 0.5217          
                                          
 Mcnemar's Test P-Value : NA              

Statistics by Class:

                     Class: X1 Class: X2 Class: X3 Class: X5 Class: X6
Sensitivity             0.6667    0.5455   0.40000   1.00000   1.00000
Specificity             0.8250    0.8974   0.87500   0.96552   0.98305
Pos Pred Value          0.6667    0.7500   0.22222   0.60000   0.66667
Neg Pred Value          0.8250    0.7778   0.94231   1.00000   1.00000
Prevalence              0.3443    0.3607   0.08197   0.04918   0.03279
Detection Rate          0.2295    0.1967   0.03279   0.04918   0.03279
Detection Prevalence    0.3443    0.2623   0.14754   0.08197   0.04918
Balanced Accuracy       0.7458    0.7214   0.63750   0.98276   0.99153
                     Class: X7
Sensitivity            0.75000
Specificity            0.98113
Pos Pred Value         0.85714
Neg Pred Value         0.96296
Prevalence             0.13115
Detection Rate         0.09836
Detection Prevalence   0.11475
Balanced Accuracy      0.86557
```

##

## ANÁLISE DE PERFORMANCE

<p style="text-align: justify">

\

</p>


```r
models <- list(original_inicial = model_rf_m1,
               original_2a = model_rf_m2aori,
               original_2b = model_rf_m2bori,
               original_3c = model_rf_m2cori,

               over_inicial = model_rf_up_m1,
               over_2a = model_rf_up_m2aup,
               over_2b = model_rf_m2bori,
               over_2c = model_rf_up_m2cup,

               under_inicial = model_rf_down_m1,
               under_2a = model_rf_down_m2aunder,
               under_2b = model_rf_down_m2bunder,
               under_2c = model_rf_down_m2cunder)

comparacao <- data.frame(Modelo = names(models),
                         Kappa  = rep(NA, length(models)),
                         Acuracia  = rep(NA, length(models)))

comparacao <- comparacao %>%
  dplyr::mutate(Acuracia = c(cm_original_m1$overall[1],
                             cm_original_m2aori$overall[1],
                             cm_original_m2bori$overall[1],
                             cm_original_m2cori$overall[1],
                                                          
                             cm_over_m1$overall[1],
                             cm_over_m2aup$overall[1],
                             cm_over_m2bup$overall[1],
                             cm_over_m2cup$overall[1],
                             
                             cm_down_m1$overall[1],
                             cm_down_m2aunder$overall[1],
                             cm_down_m2bunder$overall[1],
                             cm_down_m2cunder$overall[1]),
                
                Kappa    = c(cm_original_m1$overall[2],
                             cm_original_m2aori$overall[2],
                             cm_original_m2bori$overall[2],
                             cm_original_m2cori$overall[2],
                             
                             cm_over_m1$overall[2],
                             cm_over_m2aup$overall[2],
                             cm_over_m2bup$overall[2],
                             cm_over_m2cup$overall[2],
                             
                             cm_down_m1$overall[2],
                             cm_down_m2aunder$overall[2],
                             cm_down_m2bunder$overall[2],
                             cm_down_m2cunder$overall[2]
                             )) %>%
  reshape2::melt(id.vars = c("Modelo"))

levels(comparacao$variable) <- c("Kappa", "Acurácia")

shiny::div(plotly::ggplotly(ggplot2::ggplot(comparacao,
                                            (aes(x = variable,
                                                 y = value,
                                                 color = Modelo))) +
                              geom_jitter(width = 0.2, alpha = 0.5, size = 3) +
                              scale_y_continuous(limits = c(0, 1)) +
                              labs(title = "Classificação Vidro",
                                   x = "Índice",
                                   y = "Valor",
                                   color = "Legenda") +
                              theme_bw() +
                              theme(axis.text.x = element_text(angle = 90, hjust = 1))),
           align = "center")
```

preservea5fe85eb5bec1bee



```r
comparacao <- comparacao %>% 
  dplyr::filter(Modelo %in% c("original_inicial","over_inicial","over_2c"))

shiny::div(plotly::ggplotly(ggplot2::ggplot(comparacao,
                                            (aes(x = variable,
                                                 y = value,
                                                 color = Modelo))) +
                              geom_jitter(width = 0.2, alpha = 0.5, size = 3) +
                              labs(title = "Classificação Vidro",
                                   x = "Índice",
                                   y = "Valor",
                                   color = "Legenda") +
                              theme_bw() +
                              theme(axis.text.x = element_text(angle = 90, hjust = 1))),
           align = "center")
```

preserve66c13ab833ac2cb6



```r
df1 <- data.frame(cm_original_m1[["byClass"]], modelo = "cm_original_m1")
df2 <- data.frame(cm_over_m1[["byClass"]], modelo = "cm_over_m1")
df  <- data.frame(cm_over_m2cup[["byClass"]], modelo = "cm_over_m2cup")

plotx <- df %>% 
  rbind(df1, df2) %>% 
  cbind(classe = rep(paste0(rep('X', 6), setdiff(1:7, 4)),3)) %>% 
  dplyr::select(1, 2, 7, 12, 13) %>% 
  tidyr::gather("Metrica", "Valor", -modelo, -classe) %>% 
  dplyr::mutate(Metrica = as.factor(Metrica),
                Metrica = forcats::fct_recode(Metrica, "Sensibilidade" = "Sensitivity"),
                Metrica = forcats::fct_recode(Metrica, "Especificidade" = "Specificity"),
                modelo = forcats::fct_recode(modelo, "Orig. Cenário 1" = "cm_original_m1"),
                modelo = forcats::fct_recode(modelo, "Overs. Cenário 1" = "cm_over_m1"),
                modelo = forcats::fct_recode(modelo, "Overs. Cenário 2 - C" = "cm_over_m2cup"),
                modelo = forcats::fct_relevel(modelo, "Orig. Cenário 1",
                                              "Overs. Cenário 1",
                                              "Overs. Cenário 2 - C")) %>% 
  ggplot2::ggplot() +
  geom_jitter(aes(x = Metrica, y = Valor, col = modelo), alpha = 0.5) +
  facet_wrap(~classe, ncol = 6) +
  ylab("Score") +
  theme_minimal_hgrid(12) +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))
shiny::div(plotly::ggplotly(plotx),
           align = "center")
```

preserve006bb677588b38f6


<p style="text-align: justify">
\
Analisando o gráfico acima, podemos observar que os modelo com dados *oversampling* com o conjunto de dados sem transformação ou incremento de vriáveis apresentou melhor *socore* de Acurácia e *Kappa* ao que os outros modelos.  
\
Outra análise que pode ser feita, para a comparação dos modelos, é a análise da curva ROC, na qual temos plotados valores de **Sensitividade** e **Especificidade**, para diversos *thresholds*.
\
</p>


```r
### Original
predictions1 <- predict(model_rf_m1, test_data,type = "prob")
colnames(predictions1) <- paste(colnames(predictions1), "_pred_original")

### Up
predictions_up1 <- predict(model_rf_up_m1, test_data,type = "prob")
colnames(predictions_up1) <- paste(colnames(predictions_up1), "_pred_up")

### Juntando
true_label <- dummies::dummy(test_data$Tipo, sep = ".")
true_label <- data.frame(true_label)

colnames(true_label) <- gsub(".*?\\.", "", colnames(true_label))
colnames(true_label) <- paste(colnames(true_label), "_true")

final_df <- cbind(true_label, predictions1, predictions_up1)

### PLotando curva
roc_res <- multi_roc(final_df, force_diag = T)
pr_res  <- multi_pr(final_df, force_diag  = T)

plot_roc_df <- plot_roc_data(roc_res)
plot_pr_df  <- plot_pr_data(pr_res)

plot_roc_df <- plot_roc_df %>%
  mutate(alpha = ifelse(plot_roc_df$Group == "Micro" | plot_roc_df$Group == "Macro", 1, 0))

plotx <- ggplot(plot_roc_df, aes(x = 1-Specificity, y = Sensitivity)) +
  geom_path(aes(color = Group, linetype = Method,
                alpha = alpha),
            size = 1.5) +
  geom_segment(aes(x = 0, y = 0, xend = 1, yend = 1),
               colour = 'grey', linetype = 'dotdash') +
  scale_alpha(range = c(0.05, 1)) +
  xlab("1-Especificidade") +
  ylab("Sensitividade") +
  theme_bw() +
  theme(plot.title = element_text(hjust = 0.5),
        legend.justification = c(1, 0),
        legend.position = c(0.95, 0.05),
        legend.title = element_blank(),
        legend.background = element_rect(fill = NULL,
                                         size = 0.5,
                                         linetype = "solid",
                                         colour = "black"))
shiny::div(plotly::ggplotly(plotx), align = "center")
```

preserve0b1ea468ce31d154

```r
# Veriicando o AUC
# ORIGINAL
ma_o <- roc_res$AUC$original[7] %>% unlist()
mi_o <- roc_res$AUC$original[8] %>% unlist()

# OVERSAMPLING
ma_ov <- roc_res$AUC$up[7] %>% unlist()
mi_ov <- roc_res$AUC$up[8] %>% unlist()

#
ifelse(max(ma_o, ma_ov) == ma_o & max(mi_o, mi_ov) == mi_o,
       paste0("O maior valor de AUC estimado é para o modelo original"), 
       ifelse(max(ma_o, ma_ov) == ma_ov & max(mi_o, mi_ov) == mi_ov,
       paste0("O maior valor de AUC estimado é para o modelo oversampling"),
       paste0("Indeterminado")))
```

```
                                                       macro 
"O maior valor de AUC estimado é para o modelo oversampling" 
```




```r
# Importancia de variaveis por Ínice Gini
gini <- varImp(model_rf_up_m1)$importance %>%
  as.data.frame() %>%
  tibble::rownames_to_column() %>%
  dplyr::arrange(Overall) %>%
  dplyr::mutate(rowname = forcats::fct_inorder(rowname)) %>% 
  ggplot2::ggplot() +
  geom_col(aes(x = rowname, 
               y = Overall, 
               fill = rowname), 
           col = "black", show.legend = F) +
  coord_flip() +
  scale_fill_grey() +
  ggtitle("Random Forest - Oversampling - Cenário Inicial") +
  ylab("Variável") +
  theme_bw() + 
  theme(plot.title = element_text(color = "black", size = 22, face = "bold"))
shiny::div(plotly::ggplotly(gini), align = "center")
```

preserve7c93f2353b974942

```r
# OOB
plot_oob <- model_rf_up_m1$finalModel[["err.rate"]] %>% 
  as.data.frame() %>% 
  dplyr::mutate(Arvores = 1:arvores) %>% 
  tidyr::gather("Var", "Erro", -Arvores) %>% 
  ggplot2::ggplot() +
  geom_line(aes(x = Arvores, y = Erro, col = Var), 
            size = 1) +
  xlab("N° Árvores") + ylab("Erro") +
  theme_minimal_hgrid(12) + theme(legend.position = "bottom")
shiny::div(plotly::ggplotly(plot_oob), align = "center")
```

preserved1dabc7d10b09694

<p style="text-align: justify">
\
Para a construção da curva ROC, são construídos, inicialmente, modelos binomiais de uma determinada classe vs todas as outras, ex:
\
\
Modelo 1: X1 vs c(X2,X3,X5,X6,X7)
\
\
Assim, são calculados os valores de **Sensitividade** e **Especificidade** para diversos *thresholds*. Com todas as classes plotadas, é feita uma curva que é o valor médio de todas as outras em um determinado intervalo de confiança: **Macro** e **Micro** indicados no plot e com maior opacidade.
\
**Marcello**
\
**AUC - qual modelo?**
\
</p>


***

## CONSIDERAÇÕES FINAIS

<p style="text-align: justify">

aa...
\
Como sugestão para trabalhos futuros, indicamos a aplicação de técnica de modelos de misturas tendo em vista as possíveis sub-populações de distribuição. Sugerimos também a construção de modelos de super learning. 
\
</p>


***

\

**Discentes:**
\
**[Brenner Silva](http://lattes.cnpq.br/8005291514473676)**;
\
**[Marcello Pessoa](http://lattes.cnpq.br/)**.

\
**Docente:**
\
**[Karla Esquerre](http://lattes.cnpq.br/1956096628005272)**.

\

[<img src="http://gamma.ufba.br/images/logoRStudio.png" height="300px" width="1005px" />](http://gamma.ufba.br/)
***
\

<div class="tocify-extend-page" data-unique="tocify-extend-page" style="height: 0;"></div>


