###################################
###  PROYECTO DE ANALITICA 2024 ###
###################################
#
## Integrantes:
## Lucia Maceiras  Nº 315647
## Carolina Perez  Nº 172866
## Simone Rivero   Nº 277134
install.packages("corrplot")

## Se cargan librerias
library(arrow)
library(flextable)
library(tidyverse)
library(corrplot)
library(dlookr)
library(pROC)
library(rpart)
library(rattle)
library(xgboost)
library(dplyr)
library(tidyr)
library(ggplot2)
library(caret)
library(lattice)

## Se define el directorio de trabajo
source('C:/Users/siimo/OneDrive/Escritorio/Obligatorio/mis_funciones.r')

setwd("C:/Users/siimo/OneDrive/Escritorio/Obligatorio")
getwd()


##Se carga base de datos original en formato .ftr
# train <- read_feather("train_data.ftr")
# head(train)
 
#############################################################################
## Reduccion de la base de datos a tamaño y formato manejable ###############
#############################################################################
#
## Se verifican clases de las variables
# sapply(train,class)
# str(train, list.len=1000)
# 
# 
## Se Elimina la columna Fecha 
# Train2 <- train [, -c(2)]
# 
## Se eliminan variables según análisis previo, manteniendo las 40 variables más relevantes de las 75 disponibles
# 
## Lista de columnas que deseas mantener
# columnas_deseadas <- c("customer_ID", 'B_7', 'B_23', 'B_1', 'B_11', 'B_36', 'B_15', 'S_19', 'B_14', 'D_104',
#                        'D_77', 'D_62', 'D_103', 'P_2', 'B_25', 'B_28', 'B_27', 'D_48',
#                        'B_3', 'R_2', 'R_23', 'D_133', 'D_58', 'S_7', 'D_131', 'R_1', 'D_43', 'B_9', 'D_139',
#                        'S_23', 'B_2', 'D_118', 'R_24', 'D_121', 'D_55', 'B_18', 'D_143', 'S_25', 'R_15', 
#                        'D_141', 'R_27','target' )
# 
## Crear un nuevo dataframe solo con las columnas deseadas
# Train3 <- subset(Train2, select = columnas_deseadas)
# 
# write_feather(Train3, "Train3.ftr")
# 
## Se separan las variables mencionadas en bases más pequeñas
# variables1 <- c('customer_ID','B_7', 'B_23', 'B_1', 'B_11', 'B_36')
# variables2 <- c('customer_ID','B_15', 'S_19', 'B_14', 'D_104', 'D_77')
# variables3 <- c('customer_ID','D_62', 'D_103', 'P_2', 'B_25', 'B_28')
# variables4 <- c('customer_ID','B_27', 'D_48', 'B_3', 'R_2','R_23')
# variables5 <- c('customer_ID','D_133', 'D_58', 'S_7', 'D_131', 'R_1')
# variables6 <- c('customer_ID','D_43', 'B_9', 'D_139', 'S_23', 'B_2')
# variables7 <- c('customer_ID','D_118', 'R_24', 'D_121', 'D_55', 'B_18')
# variables8 <- c('customer_ID','D_143', 'S_25', 'R_15', 'D_141', 'R_27', 'target')
# 
# Train3.1 <- Train3[, variables1]
# Train3.2 <- Train3[, variables2]
# Train3.3 <- Train3[, variables3]
# Train3.4 <- Train3[, variables4]
# Train3.5 <- Train3[, variables5]
# Train3.6 <- Train3[, variables6]
# Train3.7 <- Train3[, variables7]
# Train3.8 <- Train3[, variables8]
# 
## Se calcula promedio por customer_ID
# average_train3.1 <- Train3.1 %>%
#   group_by(customer_ID) %>%
#   summarise(across(everything(), ~mean(., na.rm = TRUE)))
# 
# average_train3.2 <- Train3.2%>%
#   group_by(customer_ID) %>%
#   summarise(across(everything(), ~mean(., na.rm = TRUE)))
# 
# average_train3.3 <- Train3.3%>%
#   group_by(customer_ID) %>%
#   summarise(across(everything(), ~mean(., na.rm = TRUE)))
# 
# average_train3.4 <- Train3.4%>%
#   group_by(customer_ID) %>%
#   summarise(across(everything(), ~mean(., na.rm = TRUE)))
# 
# average_train3.5 <- Train3.5%>%
#   group_by(customer_ID) %>%
#   summarise(across(everything(), ~mean(., na.rm = TRUE)))
# 
# average_train3.6 <- Train3.6%>%
#   group_by(customer_ID) %>%
#   summarise(across(everything(), ~mean(., na.rm = TRUE)))
# 
# average_train3.7 <- Train3.7%>%
#   group_by(customer_ID) %>%
#   summarise(across(everything(), ~mean(., na.rm = TRUE)))
# 
# average_train3.8 <- Train3.8%>%
#   group_by(customer_ID) %>%
#   summarise(across(everything(), ~mean(., na.rm = TRUE)))
# 
## Se Elimina la columna Customer_ID de todas las subtablas excepto la primera (Train2.1)
# average_train3.2 <- average_train3.2[, -1]
# average_train3.3 <- average_train3.3[, -1]
# average_train3.4 <- average_train3.4[, -1]
# average_train3.5 <- average_train3.5[, -1]
# average_train3.6 <- average_train3.6[, -1]
# average_train3.7 <- average_train3.7[, -1]
# average_train3.8 <- average_train3.8[, -1]
# 
## Se unen las tablas
# Train4 <- cbind(average_train3.1, average_train3.2, average_train3.3, average_train3.4, 
#                        average_train3.5, average_train3.6, average_train3.7, average_train3.8)
# 
## Se Guarda la base Train4 como archivo feather
# write_feather(Train4, "Train4.ftr")
#
## En base a análisis y modelado posterior se seleccionan las 10 variables más relevantes 
## dada la limitante de memoria para trabajar con un numero mayor
#
#
# Train5 <- Train4 [, c (1:11,42)]
# head(Train5)
# write_feather(Train5, "Train5.ftr")

#############################################################################
########################## Analisis exploratorio ############################
#############################################################################

Train5 <- read_feather("Train5.ftr")

## Clases de las variables
str(Train5)
summary(Train5)

## Se revisa % default en la base
## Se calcula el porcentaje de valores igual a 1 en la columna "target"
porcentaje_default <- mean(Train5$target == 1) * 100

## Numero de Default en base Train5
sum_default <- sum(Train5$target == 1)

## Numero de No Default en Train 5
sum_nodefault <- nrow(Train5)- sum_default

## Se cuenta con datos de 458.913 clientes
## Se cuenta con 118.828 Default "1" y 340.085 No Deafult "0"

## En la base los valores "0" se encuentran submuestreada al 5% respecto al número real en AMEX.
## Cantidad Real de No Default "0" en base AMEX
no_default_real <- sum_nodefault*20

## Se cuenta con 6.801.700 No Default sin considerar el submuestreo de la base.

## Porcentajes de Default y No Default en Base Real
porcentaje_default_real <- sum_default/ (sum_default + no_default_real) *100
porcentaje_no_default_real <- 100 - porcentaje_default_real

porcentajes_reales <- c(porcentaje_default_real, porcentaje_no_default_real)

## Grafico de barras para base Real
bpreal <- barplot(c(porcentaje_default_real, 100 - porcentaje_default_real),
              names.arg = c("Porcentaje de 1", "Porcentaje de 0"),
              col = c("darkorange", "skyblue"),
              main = "Porcentaje de valores en la columna 'target'",
              ylab = "Porcentaje",
              ylim = c(0, 110),
              beside = TRUE) # Para mostrar las barras una al lado de la otra
text(x = bpreal, y = porcentajes_reales, labels = paste0(round(porcentajes_reales, 2)," %"), pos = 3, cex = 0.8, col = "black")

## Se cuenta con un 1.72% de valores "1" Default y un 98.28% de valores "0" No Default.

porcentajes <- c (porcentaje_default, 100- porcentaje_default)

## Gráfico de barras para base de trabajo
bp <- barplot(c(porcentaje_default, 100 - porcentaje_default),
        names.arg = c("Porcentaje de 1", "Porcentaje de 0"),
        col = c("darkorange", "skyblue"),
        main = "Porcentaje de valores en la columna 'target'",
        ylab = "Porcentaje",
        ylim = c(0, 100),
        beside = TRUE) # Para mostrar las barras una al lado de la otra
text(x = bp, y = porcentajes, labels = paste0(round(porcentajes, 2)," %"), pos = 3, cex = 0.8, col = "black")

## Se cuenta con un 25.89% de valores "1" Default y un 74.11% de valores "0" No Default.

## Se cuenta con una base desbalanceada, se busca un método para balancearla

## N° de clientes únicos
clientes_unicos <- unique(Train5$customer_ID)
total_clientes_unicos <- length(clientes_unicos)

## Imprime el total de clientes únicos
print(total_clientes_unicos)

## Se confirma que no hay clientes repetidos

#############################################################################
########################## Analisis descriptivo #############################
#############################################################################

Train5 <- read_feather("Train5.ftr")

## Analisis integral
diagnose_numeric(Train5) %>% flextable()

## Correlaciones
#install.packages("Hmisc")

# Cargar el paquete Hmisc
library(Hmisc)
install.packages("corplot")

par(mfrow=c(1,1))
correlaciones <- Train5 [,] %>% 
  select_if(is.numeric) %>% as.matrix() %>% rcorr(type = 'spearman')

corrplot(correlaciones$r,
         p.mat = correlaciones$r,
         method="circle",
         type="upper",
         order="FPC",
         tl.col="black",
         tl.srt = 20,
         pch.col = "blue",
         insig = "p-value",
         sig.level = -1,
         col = terrain.colors(100))
##
#D_77, B_11, B_23, B_7, B_1



## Bivariado categorico 
colnames(Train5)

par(mfrow = c(2, 2))
hist(Train5$B_7, col = 'red', main = 'B_7')
boxplot(Train5$B_7, col = 'red', main = 'B_7')
hist(Train5$B_7[Train5$B_7 <= 25000], col = 'red', main = 'B_7 acotado en 25.000')
boxplot(Train5$B_7[Train5$B_7 <= 25000], col = 'red', main = 'B_7 acotado en 25.000')

par(mfrow = c(2, 2))
hist(Train5$B_23, col = 'red', main = 'B_23')
boxplot(Train5$B_23, col = 'red', main = 'B_23')
hist(Train5$B_23[Train5$B_23 >= 3000], col = 'red', main = 'B_23 acotado en 3.000')
boxplot(Train5$B_23[Train5$B_23 >= 3000], col = 'red', main = 'B_23 acotado en 3.000')

par(mfrow = c(2, 2))
hist(Train5$B_1, col = 'red', main = 'B_1')
boxplot(Train5$B_1, col = 'red', main = 'B_1')
hist(Train5$B_1[Train5$B_1 <= 20000], col = 'red', main = 'B_1 acotado en 20.000')
boxplot(Train5$B_1[Train5$B_1 <= 20000], col = 'red', main = 'B_1 acotado en 20.000')

par(mfrow = c(2, 2))
hist(Train5$B_11, col = 'red', main = 'B_11')
boxplot(Train5$B_11, col = 'red', main = 'B_11')
hist(Train5$B_11[Train5$B_11 >= 2000], col = 'red', main = 'B_11 acotado en 2.000')
boxplot(Train5$B_11[Train5$B_11 >= 2000], col = 'red', main = 'B_11 acotado en 2.000')

par(mfrow = c(2, 2))
hist(Train5$B_36, col = 'red', main = 'B_36')
boxplot(Train5$B_36, col = 'red', main = 'B_36')
hist(Train5$B_36[Train5$B_36 > 4000 & Train5$B_36 <= 12000], col = 'red', main = 'B_36 acotado en 4.000 & 12.000')
boxplot(Train5$B_36[Train5$B_36 > 4000 & Train5$B_36 <= 12000], col = 'red', main = 'B_36 acotado en 4.000 & 12.000')

par(mfrow = c(2, 2))
hist(Train5$B_15, col = 'red', main = 'B_15')
boxplot(Train5$B_15, col = 'red', main = 'B_15')
hist(Train5$B_15[Train5$B_15 <= 20000], col = 'red', main = 'B_15 acotado en 20.000')
boxplot(Train5$B_15[Train5$B_15 <= 20000], col = 'red', main = 'B_15 acotado en 20.000')

par(mfrow = c(2, 2))
hist(Train5$S_19, col = 'red', main = 'S_19')
boxplot(Train5$S_19, col = 'red', main = 'S_19')
hist(Train5$S_19[Train5$S_19 > 4000 & Train5$S_19 <= 10000], col = 'red', main = 'S_19 acotado en 4.000 & 10.000')
boxplot(Train5$S_19[Train5$S_19 > 4000 & Train5$S_19 <= 10000], col = 'red', main = 'S_19 acotado en 4.000 & 10.000')

par(mfrow = c(2, 2))
hist(Train5$B_14, col = 'red', main = 'B_14')
boxplot(Train5$B_14, col = 'red', main = 'B_14')
hist(Train5$B_14[Train5$B_14 <= 25000], col = 'red', main = 'B_14 acotado en 25.000')
boxplot(Train5$B_14[ Train5$B_14 <= 25000], col = 'red', main = 'B_14 acotado en 25.000')

par(mfrow = c(2, 2))
hist(Train5$D_104, col = 'red', main = 'D_104')
boxplot(Train5$D_104, col = 'red', main = 'D_104')
hist(Train5$D_104[Train5$D_104 > 2500], col = 'red', main = 'D_104 acotado en 2.500')
boxplot(Train5$D_104[ Train5$D_104 > 2500], col = 'red', main = 'D_104 acotado en 2.500')

par(mfrow = c(2, 2))
hist(Train5$D_77, col = 'red', main = 'D_77')
boxplot(Train5$D_77, col = 'red', main = 'D_77')
hist(Train5$D_77[Train5$D_77 >= 5000], col = 'red', main = 'D_77 acotado en 5.000')
boxplot(Train5$D_77[Train5$D_77 >= 5000], col = 'red', main = 'D_77 acotado en 5.000')


#############################################################################
########################## Tratamiento de datos #############################
#############################################################################

Train5 <- read_feather("Train5.ftr")

## Se eliminan de la base Train5 clientes NA's, dada la enorme cantidad de datos disponibles (458.913)
Train_clean <- na.omit(Train5)
## La base limpia se compone por datos de 279.141 clientes

porcentaje_default <- mean(Train_clean$target == 1) * 100
porcentaje_default
## La base limpia tiene un porcentaje de default de 23.93%

## Se submuestrean y balancean (50/50) los datos
## Se separan filas con target 0 y target 1
Train_clean_0 <- subset(Train_clean, target == 0)
Train_clean_1 <- subset(Train_clean, target == 1)

## Se muestrean 50,000 filas al azar de cada grupo
set.seed(123)
sampled_0 <- Train_clean_0[sample(nrow(Train_clean_0), 50000), ]
sampled_1 <- Train_clean_1[sample(nrow(Train_clean_1), 50000), ]

## Se divide sampled_0 en Train y Test 
train_0 <- sample(nrow(sampled_0), nrow(sampled_0)*0.7) 
test_0 <- (-train_0)

## Se divide sampled_1 en Train y Test 
train_1 <- sample(nrow(sampled_1), nrow(sampled_1)*0.7) 
test_1 <- (-train_1)

## Se combinan las dos muestras para hacer Train
Train_sub <- rbind(sampled_0 [train_0, ], sampled_1[train_1, ])
head(Train_sub)

## Se combinan las dos muestras para hacer Test
Test_sub <- rbind(sampled_0 [test_0, ], sampled_1[test_1, ])
head(Test_sub)

## Se guarda Train en archivo .ftr
write_feather(Train_sub, "Train_sub.ftr")

## Se guarda Test en archivo .ftr
write_feather(Test_sub, "Test_sub.ftr")

## Se carga Train y Test submuestreado balanceado
Train_sub <- read_feather("Train_sub.ftr")
Test_sub <- read_feather("Test_sub.ftr")

#############################################################################
########################## Modelado #########################################
#############################################################################

## Árbol 
arbol.ini <- rpart(target ~ ., 
                   data = Train_sub[,-1 ] , 
                   method = "class", 
                   maxdepth=4,
                   cp = 0.01,
                   minbucket=20)

arbol.ini

## Se utiliza regla del codo para podar el árbol
par(mfrow=c(1,1))
plotcp(arbol.ini)

# Arbol podado
arbol.pr <- prune(arbol.ini, cp = 0.011)
arbol.pr
fancyRpartPlot(arbol.pr,split.cex = 0.8) 

##Se valida la importancia de las variables
arbol.pr$variable.importance/sum(arbol.pr$variable.importance)

## Prediccion de probabilidad 
pred_arbol_train<- predict(arbol.pr,Train_sub)[,2]

## Se guarda el modelo en un archivo .rds
saveRDS(arbol.pr, file = "arbol.rds")

## Se guarda el modelo desde el archivo .rds
arbol.pr <- readRDS("arbol.rds")

## Prediccion arbol en test
pred_arbol_test<- predict(arbol.pr,Test_sub)[,2]

#### Resumen de los resultados Train: 

Rtrain=cbind(Train_sub$target,pred_arbol_train)
Rtrain=as.data.frame(Rtrain)
colnames(Rtrain)=c('target','Arbol Podado')

## Se guarda Train en archivo .ftr
write_feather(Rtrain, "Rtrain.ftr")

#### Resumen de los resultados Test: 

Rtest=cbind(Test_sub$target,pred_arbol_test)
Rtest=as.data.frame(Rtest)
colnames(Rtest)=c('target','Arbol Podado')

## Se guarda Train en archivo .ftr
write_feather(Rtest, "Rtest.ftr")

## Se cargan Rtest y Rtrain
Rtest <- read_feather("Rtest.ftr")
Rtrain <- read_feather("Rtrain.ftr")

## Coeficiente Gini Normalizado (G):

NormalizedGini <- function(y_true, y_pred) {
roc_x <- pROC::roc(y_true, y_pred)
auc <- pROC::auc(roc_x)
gini <- 2 * auc - 1
return(gini)
}

## Tasa de Incumplimiento Capturada al 4% (D):
  
DefaultRateAt4 <- function(y_true, y_pred) {
  threshold <- quantile(y_pred, 0.96)
  return(mean(y_true[y_pred >= threshold]))
}

## Boosting
## Se carga Train_sub
Train_sub <- read_feather("Train_sub.ftr")

## Se transforma Train en dataframe para poder utilizar XGBoost y luego en formato DMatrix 
Train_sub <- as.data.frame(Train_sub)

datos_train <- xgb.DMatrix(
  data  = data.matrix(Train_sub[,c(-1,-12)]),
  label = Train_sub[,12])

## Modelado
set.seed(123)
modelo = xgb.train(data = datos_train, params = list(eta=0.05, objective = 'binary:logistic',max_depth=4,subsample = 0.8), nrounds = 30)
modelo

## Se guarda el modelo en un archivo .rds
saveRDS(modelo, file = "modelo.rds")
## Se carga el modelo desde el archivo .rds
modelo <- readRDS("modelo.rds")

## Se verifica que el modelo se haya cargado correctamente
print(modelo)

## Se busca optimizar el boosting
## Se crea un data frame de parametros de optimizacion
  optboosting <- data.frame(
    tree_depth = c(2, 4, 6, 2, 4, 6, 2, 4, 6, 2, 4, 6, 2, 4, 6, 2, 4, 6),
    learn_rate = rep(c(0.02, 0.05, 0.10), each = 6),
    sample_size = rep(c(0.8, 1.0), each = 9)
  )
  optboosting$M <- rep(NA, nrow(optboosting))
  
## Se crean pliegues de validación cruzada
  set.seed(123)
  folds <- createFolds(Train_sub$target, k = 5, list = TRUE)
  
  for (i in 1:nrow(optboosting)) {
    
    all_preds <- numeric()
    all_true <- numeric()
    
    for (fold in folds) {
      train_idx <- setdiff(seq_len(nrow(Train_sub)), fold)
      val_idx <- fold
      
      train_data <- xgb.DMatrix(data = as.matrix(Train_sub[train_idx, -c(1,12)]), label = Train_sub[train_idx, 12])
      val_data <- xgb.DMatrix(data = as.matrix(Train_sub[val_idx, -c(1,12)]), label = Train_sub[val_idx, 12])
      
      params <- list(
        eta = optboosting$learn_rate[i],
        max_depth = optboosting$tree_depth[i],
        subsample = optboosting$sample_size[i],
        objective = "binary:logistic",
        eval_metric = "auc"
      )
      set.seed(123)
      model <- xgb.train(
        params = params,
        data = train_data,
        nrounds = 100,
        watchlist = list(val = val_data),
        verbose = 0
      )
      
      preds <- predict(model, val_data)
      all_preds <- c(all_preds, preds)
      all_true <- c(all_true, Train_sub[val_idx, 12])
    }
    
    gini = NormalizedGini(all_true, all_preds)
    default_rate_at_4 = DefaultRateAt4(all_true, all_preds)
    M = 0.5 * (gini + default_rate_at_4)
    
    optboosting$M[i] <- M
  }
  
## Se encuentra el valor máximo de M y la fila correspondiente
  max_M <- max(optboosting$M, na.rm = TRUE)
  optimal_row <- which.max(optboosting$M)
  max_M
  optimal_row
  
optboosting [12,]


## Se corre el Boosting con los parametros  optimos  
modelo_optimizado = xgb.train(data = datos_train, params = list(eta=0.05, objective = 'binary:logistic',max_depth=6,subsample = 1), nrounds = 100)
modelo_optimizado

# Se guarda el modelo en un archivo .rds
saveRDS(modelo_optimizado, file = "modelo_optimizado.rds")

#Importancia de variables en XGBoost
importance_matrix <- xgb.importance(model = modelo_optimizado)
print(importance_matrix)

#############################################################################
################### Análisis de resultados ##################################
#############################################################################
## Se cargan los modelos desde el archivo .rds
modelo_optimizado <- readRDS("modelo_optimizado.rds")
modelo <- readRDS("modelo.rds")

## Se cargan bases Train y Test submuestreadas balanceadas
Train_sub <-read_feather("Train_sub.ftr")
Test_sub <- read_feather("Test_sub.ftr")

## Se convierte Train y Test en DMatrix
Train_sub <- as.data.frame(Train_sub)
Test_sub <- as.data.frame(Test_sub)

datos_train <- xgb.DMatrix(
  data  = data.matrix(Train_sub[,c(-1,-12)]),
  label = Train_sub[,12])

datos_test <- xgb.DMatrix(
  data  = data.matrix(Test_sub[,c(-1,-12)]),
  label = Test_sub[,12])

## Predicción de Boosting y Boosting Optimizado
pred_boost_opt_train = predict(modelo_optimizado,newdata = datos_train)
pred_boost_opt_test = predict(modelo_optimizado,newdata = datos_test)

pred_boost_train = predict(modelo,newdata = datos_train)
pred_boost_test = predict(modelo,newdata = datos_test)

## Resultados consolidad de modelos 
Rtest <- read_feather("Rtest.ftr")
Rtrain <- read_feather("Rtrain.ftr")

Rtest <- cbind(Rtest,pred_boost_test, pred_boost_opt_test)
Rtrain <- cbind(Rtrain,pred_boost_train, pred_boost_opt_train)

### Performance de los modelos 
performance <- function(X, Y) {
  X_bin = ifelse(X > 0.5, 1, 0)
  
  mc = addmargins(table(X_bin, Y))
  roc_x <- roc(Y, as.numeric(X), quiet = TRUE)
  
  sensitivity = mc[2, 2] / mc[3, 2]
  specificity = mc[1, 1] / mc[3, 1]
  precision = (mc[1, 1] + mc[2, 2]) / mc[3, 3]
  auc = roc_x$auc
  f1 = 2 * (sensitivity * precision) / (sensitivity + precision)
  
  gini = NormalizedGini(Y, as.numeric(X))
  default_rate_at_4 = DefaultRateAt4(Y, as.numeric(X))
  
  M = 0.5 * (gini + default_rate_at_4)
  
  r = cbind(
    Sensitividad = sensitivity,
    Especificidad = specificity,
    Precision = precision,
    AUC = auc,
    F1 = f1,
    Gini = gini,
    DefaultRateAt4 = default_rate_at_4,
    M = M
  )
  return(r)
}

### Resultados para Train
resultados.train=data.frame()

for (i in 2:ncol(Rtrain)){
  resultados.train=rbind(resultados.train,performance(Rtrain[,i],Rtrain[,1]))
}
rownames(resultados.train)=colnames(Rtrain)[-1]
resultados.train

sink('resultados.train.txt')
resultados.train
sink()

### Resultados para test
resultados.test=data.frame()

for (i in 2:ncol(Rtest)){
  resultados.test=rbind(resultados.test,performance(Rtest[,i],Rtest[,1]))
}
rownames(resultados.test)=colnames(Rtest)[-1]
resultados.test

sink('resultados.test.txt')
resultados.test
sink()
