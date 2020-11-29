library(tidyverse) # metapackage with lots of helpful functions
library(data.table)
library(lightgbm)
library(dplyr)
library(stringr)
library(tidyselect)
library(ModelMetrics)
library(ggplot2)
library(caret)
library(fastDummies)
library(catboost)
library(lubridate)
library(dataMaid)
library(tidyr)
library(zoo)
## Running code

list.files(path = "../input")
set.seed(0)
memory.limit(size=100000000)

##################
# Funciones
free <- function() invisible(gc())

##################

PATH <- "C:/Users/axovivas/Documents/Kaggle/HackatonColombia/"

####################################
# Paso 0 cargar los archivos
####################################
Clientes <- fread(paste0(PATH,"Input1_clientes_estructura.csv"))
colSums(is.na(Clientes)) ## No tiene campos en nulo

Ventas <- fread(paste0(PATH,"Input2_clientes_venta.csv"))
colSums(is.na(Ventas)) ## No tiene campos en nulo
names(Ventas)[1] <- "Ano"
names(Ventas) <- str_replace(names(Ventas),"2","")
Ventas[,AnoMes:= Ano*100+Mes]
Ventas[,`:=`(Ano=NULL, Mes=NULL)]

submission <- fread(paste0(PATH,"Input3_clientes_test.csv"))
free()
##################

################################################
# Paso 1 Generar el calendario de Abr19 a Oct20
################################################
calendar <- as.data.table(expand.grid(mes = seq(1, 12, 1),
                                      ano = seq(2019, 2020, 1)))
calendar[,AnoMes:= ano*100+mes]
#calendar[,`:=`(ano=NULL, mes=NULL)]
calendar <- calendar[AnoMes>=201905 & AnoMes<202010,]

################################################
# Paso 2 Combinar Cliente-SKU-AnoMes
################################################

# Identifico los SKUs distintos que se comercializan
Ventas$SKU <- paste(Ventas$Marca, Ventas$Cupo, Ventas$CapacidadEnvase, sep=".")

# Identifico la primera venta de cada SKU en cada cliente
PrimeraVenta <- Ventas[,.(InicVenta = min(AnoMes)), by=.(Cliente,SKU)]
PrimeraVentaCliente <- Ventas[,.(InicVentaCliente = min(AnoMes)), by=.(Cliente)]
PrimeraVentaSKU <- Ventas[,.(InicVentaSKU = min(AnoMes)), by=.(SKU)]

# Se combina los Clientes-SKU-Mes
ClienteSKUMes <- as.data.table(expand.grid(Cliente = unique(Clientes$Cliente),
                                           SKU = unique(Ventas$SKU),
                                           AnoMes = unique(calendar$AnoMes)
))

# Se incluye registra las primera venta del SKU en el cliente
ClienteSKUMes[PrimeraVenta,InicVenta:= i.InicVenta, on = .(Cliente,SKU)]

# Se eliminan los SKU en meses previos a su primera venta
ClienteSKUMes <- ClienteSKUMes[AnoMes>=InicVenta,]

# Se incluyen los clientes a predecir
# Marca1: Marca_20 - Cupo_3 - CapacidadEnvase_9
# Marca2: Marca_16- Cupo_2 - CapacidadEnvase_10
# Marca3: Marca_9-Cupo_3-CapacidadEnvase_12
# Marca_Inno1: Marca_38- Cupo_2 -CapacidadEnvase_10
# Marca_Inno2: Marca_39-Cupo_2-CapacidadEnvase_10
ClienteSKUPred <- as.data.table(expand.grid(Cliente = unique(submission$Cliente),
                                            SKU = c('Marca_20.Cupo_3.CapacidadEnvase_9',
                                                    'Marca_16.Cupo_2.CapacidadEnvase_10',
                                                    'Marca_9.Cupo_3.CapacidadEnvase_12',
                                                    'Marca_38.Cupo_2.CapacidadEnvase_10',
                                                    'Marca_39.Cupo_2.CapacidadEnvase_10'
                                            ),
                                            AnoMes = 202010 ))
ClienteSKUPred[PrimeraVenta,InicVenta:= i.InicVenta, on = .(Cliente,SKU)]
ClienteSKUPred[is.na(InicVenta),InicVenta:=202010] ## Inicializo las ventas desde 202010 a pesar que no se logre

ClienteSKUMes <- rbind(ClienteSKUMes,ClienteSKUPred)
ClienteSKUMes$SKU <- as.character(ClienteSKUMes$SKU)


# Se incluye registra las primera venta del cliente
ClienteSKUMes[PrimeraVentaCliente,InicVentacliente:= i.InicVentaCliente, on = .(Cliente)]
ClienteSKUMes[PrimeraVentaSKU,InicVentaSKU:= InicVentaSKU, on = .(SKU)]
ClienteSKUMes[,MesesConVentaCliente:=ifelse(AnoMes-InicVentacliente>88,AnoMes-InicVentacliente-88,AnoMes-InicVentacliente)]
ClienteSKUMes[,MesesConVentaSKU:=ifelse(AnoMes-InicVentaSKU>88,AnoMes-InicVentaSKU-88,AnoMes-InicVentaSKU)]

# Combino la data de Ventas
ClienteSKUMes[Ventas,
              `:=`(
                  Marca = i.Marca,
                  Cupo = i.Cupo,
                  CapacidadEnvase = i.CapacidadEnvase,
                  SegmentoPrecio=i.SegmentoPrecio,
                  Volumen =i.Volumen,
                  disc = i.disc,
                  nr = i.nr)
              ,on=.(Cliente,AnoMes,SKU)]

# Combino la data de Año y Mes por separado
ClienteSKUMes[calendar,
              `:=`(
                  Ano = i.ano,
                  Mes = i.mes)
              ,on=.(AnoMes)]
rm(calendar)
free()
ClienteSKUMes[AnoMes==202010,Ano:= 2020]
ClienteSKUMes[AnoMes==202010,Mes:= 10]

# Coloco cero en los registros de ventas vacios y el segmento precio más repetido en el SKU
cols <- c("Volumen", "disc", "nr")   
ClienteSKUMes[is.na(Volumen), (cols) := lapply(.SD, function(x) 0), .SDcols = cols]
ClienteSKUMes[ ClienteSKUMes[, names(sort(-table(Marca)))[1],by = SKU], 
               Marca := ifelse(is.na(Marca), V1, Marca)
               , on = "SKU"]
ClienteSKUMes[ ClienteSKUMes[, names(sort(-table(Cupo)))[1],by = SKU], 
               Cupo := ifelse(is.na(Cupo), V1, Cupo)
               , on = "SKU"]
ClienteSKUMes[ ClienteSKUMes[, names(sort(-table(CapacidadEnvase)))[1],by = SKU], 
               CapacidadEnvase := ifelse(is.na(CapacidadEnvase), V1, CapacidadEnvase)
               , on = "SKU"]
ClienteSKUMes[ ClienteSKUMes[, names(sort(-table(SegmentoPrecio)))[1],by = SKU], 
               SegmentoPrecio := ifelse(is.na(SegmentoPrecio), V1, SegmentoPrecio)
               , on = "SKU"]

# Calculo los meses transcurridos desde la primera venta
ClienteSKUMes[,MesesConVenta:=ifelse(AnoMes-InicVenta>88,AnoMes-InicVenta-88,AnoMes-InicVenta)]

# Calculo el precio por litro
#ClienteSKUMes[,PrecioLitro:=ifelse(Volumen>0,(nr+disc)/Volumen,0)]

# Combinamos los resultados con la data de los Clientes
ClienteSKUMes[Clientes,`:=`(Regional=i.Regional2,
                            Gerencia=i.Gerencia2,
                            SubCanal=i.SubCanal2,
                            Categoria=i.Categoria,
                            Nevera=i.Nevera
),on= "Cliente"]

# Despejamos la variable objetivo "HUBO O NO HUBO VENTA"
ClienteSKUMes[,ConVenta:=ifelse(Volumen>0,1,0)]


# Feature Engineering
ClienteSKUMes <- ClienteSKUMes[order(Cliente,SKU,AnoMes)]

# Calculo los clientes con Venta mes a mes del SKU
ClienteSKUMes[,ClientesConVenta:=sum(ConVenta), by=.(AnoMes,SKU)]
ClienteSKUMes[,ClientesConVenta_lag1 := shift(ClientesConVenta,1,"lag"), by=.(Cliente,SKU)]

ClienteSKUMes[,ConVenta_lag1 := shift(ConVenta,1,"lag"), by=.(Cliente,SKU)]
ClienteSKUMes[, rmean_3_meses:= frollmean(ConVenta_lag1, 3, na.rm = TRUE), by = .(Cliente,SKU)]
ClienteSKUMes[, rmean_6_meses:= frollmean(ConVenta_lag1, 6, na.rm = TRUE), by = .(Cliente,SKU)]

ClienteSKUMes[,Vol_lag1 := shift(Volumen,1,"lag"), by=.(Cliente,SKU)]
ClienteSKUMes[,Vol_lag1 := log1p(Vol_lag1)]
#ClienteSKUMes[, rmeanVol_3_meses:= frollmean(Vol_lag1, 3, na.rm = TRUE), by = .(Cliente,SKU)]
#ClienteSKUMes[,varVol := ifelse(rmeanVol_3_meses==0,1,Vol_lag1/rmeanVol_3_meses)-1]
#ClienteSKUMes[, rmeanVol_6_meses:= frollmean(Vol_lag1, 6, na.rm = TRUE), by = .(Cliente,SKU)]
#ClienteSKUMes[, TendVol:= ifelse(rmeanVol_6_meses==0,1,rmeanVol_3_meses/rmeanVol_6_meses)]

# ClienteSKUMes[,MontoTransc:=nr-disc]
# 
# ClienteSKUMes[Volumen>0,Dcto:= -disc/(nr-disc)]
# ClienteSKUMes %>%
#     group_by(SKU) %>%
#     summarise(mean(Dcto,na.rm=T),
#               min(Dcto,na.rm=T),
#               max(Dcto,na.rm=T),
#               sd(Dcto,na.rm=T)
#     )
# ClienteSKUMes <- ClienteSKUMes %>% fill(Dcto,.direction = "down")
# ClienteSKUMes[is.na(Dcto),Dcto:=0]

# ClienteSKUMes[Volumen>0,Dcto:= -disc/(nr-disc)]
# ClienteSKUMes <- as.data.table(ClienteSKUMes %>% group_by(Cliente,SKU) %>% mutate(Dcto = zoo::na.locf(Dcto, na.rm = FALSE)))
# ClienteSKUMes[ ClienteSKUMes[!is.na(Dcto), mean(Dcto),by = .(SKU,SubCanal,AnoMes)], 
#                Dcto := ifelse(is.na(Dcto), V1, Dcto)
#                , on = c("SKU","SubCanal","AnoMes")]
# ClienteSKUMes[ ClienteSKUMes[!is.na(Dcto), mean(Dcto),by = .(SKU)], 
#                Dcto := ifelse(is.na(Dcto), V1, Dcto)
#                , on = c("SKU")]

ClienteSKUMes[Volumen>0,PrecioLitro:= (nr-disc)/Volumen]
ClienteSKUMes <- as.data.table(ClienteSKUMes %>% group_by(Cliente,SKU) %>% mutate(PrecioLitro = zoo::na.locf(PrecioLitro, na.rm = FALSE)))
ClienteSKUMes[ ClienteSKUMes[!is.na(PrecioLitro), mean(PrecioLitro),by = .(SKU,SubCanal,AnoMes)], 
               PrecioLitro := ifelse(is.na(PrecioLitro), V1, PrecioLitro)
               , on = c("SKU","SubCanal","AnoMes")]
ClienteSKUMes[ ClienteSKUMes[!is.na(PrecioLitro), mean(PrecioLitro),by = .(SKU)], 
               PrecioLitro := ifelse(is.na(PrecioLitro), V1, PrecioLitro)
               , on = c("SKU")]


ClienteSKUMes[Volumen>0,PrecioLitroCliente:= (nr)/Volumen]
ClienteSKUMes <- as.data.table(ClienteSKUMes %>% group_by(Cliente,SKU) %>% mutate(PrecioLitroCliente = zoo::na.locf(PrecioLitroCliente, na.rm = FALSE)))
ClienteSKUMes[ ClienteSKUMes[!is.na(PrecioLitroCliente), mean(PrecioLitroCliente),by = .(SKU,SubCanal,AnoMes)], 
               PrecioLitroCliente := ifelse(is.na(PrecioLitroCliente), V1, PrecioLitroCliente)
               , on = c("SKU","SubCanal","AnoMes")]
ClienteSKUMes[ ClienteSKUMes[!is.na(PrecioLitroCliente), mean(PrecioLitroCliente),by = .(SKU)], 
               PrecioLitroCliente := ifelse(is.na(PrecioLitroCliente), V1, PrecioLitroCliente)
               , on = c("SKU")]

# ClienteSKUMes[Volumen>0,PrecioLitro:= (nr-disc)/Volumen]
# ClienteSKUMes[Volumen>0,PrecioLitroCliente:= (nr)/Volumen]
# ClienteSKUMes <- ClienteSKUMes %>% group_by(Cliente,SKU) %>% fill(PrecioLitro)
# ClienteSKUMes <- ClienteSKUMes %>% group_by(Cliente,SKU) %>% fill(PrecioLitroCliente)

# ClienteSKUMes %>%
#     group_by(SKU) %>%
#     summarise(mean(PrecioLitro,na.rm=T),
#               min(PrecioLitro,na.rm=T),
#               max(PrecioLitro,na.rm=T),
#               sd(PrecioLitro,na.rm=T)
#               )

#ClienteSKUMes <- ClienteSKUMes %>% fill(PrecioLitro,.direction = "down")


dt <- ClienteSKUMes[,.(AnoMes,Cliente,SKU,
                       Marca, Cupo, CapacidadEnvase,SegmentoPrecio,Gerencia,SubCanal,Categoria, ##Regional
                       MesesConVenta, MesesConVentaCliente, MesesConVentaSKU,
                       Nevera, ClientesConVenta_lag1,
                       ConVenta_lag1, rmean_3_meses, rmean_6_meses, ##rmean_9_meses, rmean_12_meses, rmean_16_meses,
                       #Vol_lag1, rmeanVol_3_meses, rmeanVol_6_meses,
                       Vol_lag1, #varVol, #Dcto, #PrecioLitro,
                       PrecioLitro, PrecioLitroCliente, ##Dcto
                       Ano, Mes,
                       ConVenta)]
cols <- c("Marca", "Cupo", "CapacidadEnvase","SegmentoPrecio","Gerencia","SubCanal","Categoria")   ##"Regional"
dt[, (cols) := lapply(.SD, function(x) as.integer(factor(x))), .SDcols = cols]


rm(Ventas)
rm(ClienteSKUPred)
rm(PrimeraVenta)
rm(PrimeraVentaCliente)
rm(Clientes)
free()

#############################################
library(fastDummies)
cats <- c("Cupo","SegmentoPrecio","Gerencia","Categoria","Mes")
dt <- dummy_cols(dt,  select_columns = cats)
names(dt) <- str_replace(names(dt), c("[.& ]"), "")

dt[,Marca_TargetEnc:=mean(ConVenta, na.rm=T),by=Marca]
dt[,CapacidadEnvase_TargetEnc:=mean(ConVenta, na.rm=T),by=CapacidadEnvase]
dt[,SubCanal_TargetEnc:=mean(ConVenta, na.rm=T),by=SubCanal]

# dt[,Cupo_TargetEnc:=mean(ConVenta, na.rm=T),by=Cupo]
# dt[,SegmentoPrecio_TargetEnc:=mean(ConVenta, na.rm=T),by=SegmentoPrecio]
# dt[,Gerencia_TargetEnc:=mean(ConVenta, na.rm=T),by=Gerencia]
# dt[,Categoria_TargetEnc:=mean(ConVenta, na.rm=T),by=Categoria]

#############################################

train <- dt[AnoMes<202010,]
train <- na.omit(train)
y <- train$ConVenta
idx <- createDataPartition(y, p = 0.8, list = F)
train <- train[,-c("ConVenta","AnoMes","Cliente","SKU")]
dataMatrix <- data.matrix(train)
rm(ClienteSKUMes)
free()

kfolds = 5
set.seed(2503)
folds <- createFolds(y, k = kfolds, list = FALSE)

predLGBM <- dt[AnoMes==202010,ConVenta] * 0
# for (iFold in 1:kfolds)
# {
#     idx <- which(folds!=iFold)
#     free()
     xtr <- lgb.Dataset(dataMatrix[idx, ], label = y[idx], categorical_feature = cols) # construct lgb dataset
     xval <- lgb.Dataset(dataMatrix[-idx, ], label = y[-idx], categorical_feature = cols)
     set.seed(2503)
     #p <- list(objective = "binary",  metric ="auc", max_depth = -1)
     p <- list(objective = "binary",  metric ="auc", learning_rate = 0.1, nthread = 8) ##0.075
     m_lgb <- lgb.train(params = p, data = xtr, nrounds = 2000, valids = list(train=xtr, val = xval),
                        early_stopping_rounds = 50, verbose = 1, eval_freq = 100)
     cat("Best score:", m_lgb$best_score, "at", m_lgb$best_iter, "iteration")  
     predAux <- predict(m_lgb, data.matrix(dt[AnoMes==202010,-c("ConVenta","AnoMes","Cliente","SKU")]))   
     predLGBM <- predLGBM + predAux
# }
# pred2 <- pred2/5

predCB <- dt[AnoMes==202010,ConVenta] * 0
#for (iFold in 1:kfolds)
for (iFold in 1:1)
{
    #idx <- which(folds!=iFold)
    trainpool <- catboost.load_pool(train[idx, ], label = y[idx], cat_features = cols)
    testpool <- catboost.load_pool(train[-idx, ], label = y[-idx], cat_features = cols) ## cat_features = cats)
    params <- list(iterations = 1000, eval_metric = 'AUC', task_type = 'CPU', random_seed=2503, metric_period=100,
                   verbose = 1, use_best_model = T, od_type = 'Iter', od_wait = 10)
    cat_model <- catboost.train( learn_pool = trainpool, test_pool = testpool, params = params)
    test <- dt[AnoMes==202010,-c("ConVenta","AnoMes","Cliente","SKU")]
    predAux <- catboost.predict(cat_model, catboost.load_pool(test))
    predCB <- predCB + predAux
}
#pred <- pred/5


if (1==2) {
    dImp <- catboost.get_feature_importance(cat_model, 
                                            pool = testpool, 
                                            type = 'FeatureImportance',
                                            thread_count = -1)
    
    dImp <- as.data.frame(dImp)
    dImp
}

#pred <- predCB
pred <- predCB*0 + predLGBM*1
#ClienteSKUMes <- ClienteSKUMes[order(Cliente,SKU,AnoMes)]
#ClienteSKUPred <- ClienteSKUPred[order(Cliente,SKU,AnoMes)]
submit <- dt[AnoMes==202010,]
submit$pred <- pred

submission$Marca1 <- as.numeric(submission$Marca1)
submission[submit[SKU=='Marca_20.Cupo_3.CapacidadEnvase_9',.(Cliente,pred)],
           Marca1 := pred,
           on = "Cliente"]

submission$Marca2 <- as.numeric(submission$Marca2)
submission[submit[SKU=='Marca_16.Cupo_2.CapacidadEnvase_10',.(Cliente,pred)],
           Marca2 := pred,
           on = "Cliente"]

submission$Marca3 <- as.numeric(submission$Marca3)
submission[submit[SKU=='Marca_9.Cupo_3.CapacidadEnvase_12',.(Cliente,pred)],
           Marca3 := pred,
           on = "Cliente"]

submission$Marca_Inno1 <- as.numeric(submission$Marca_Inno1)
submission[submit[SKU=='Marca_38.Cupo_2.CapacidadEnvase_10',.(Cliente,pred)],
           Marca_Inno1 := pred,
           on = "Cliente"]

submission$Marca_Inno2 <- as.numeric(submission$Marca_Inno2)
submission[submit[SKU=='Marca_39.Cupo_2.CapacidadEnvase_10',.(Cliente,pred)],
           Marca_Inno2 := pred,
           on = "Cliente"]

fwrite(submission, paste0(PATH,"Envios/BeerAnalytics_Final.csv"))
