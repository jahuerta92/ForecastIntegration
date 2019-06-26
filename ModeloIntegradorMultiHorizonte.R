library(lubridate)
library(data.table)
library(foreach)
library(dplyr)
library(caret)

set.seed(0)

#CARPETAS DE DESTINO #################################################################################################################
MODDIR <- "modelos" 
RESDIR <- "resultados"
PLTDIR <- "graficas"

#CARGAR DATOS DE SEVILLA #############################################################################################################
predictionData <- readRDS("predicciones.rds")
sevilleData <- predictionData$sevilla
rm(predictionData)

#ELEGIR DATOS DE ENTRADA #############################################################################################################
#fileID <- "fourInputsMissingGHI"
#radiationType   <- "ghi"
useKt <- T
useNA <- F
radiationCs     <- paste(radiationType,"Cs",sep="")
radiationKt     <- paste(radiationType,"Kt",sep="")
if(useKt) radiationType <- radiationKt
predictionType  <- c("CIADCast","Satelite","SmartPersistence","WRFsolar")
#predictionType  <- c("WRFsolar")
#predictionType  <- c("CIADCast")
#predictionType  <- c("Satelite")
#predictionType  <- c("SmartPersistence")
features        <- paste(radiationType,predictionType,sep="_")
target          <- paste(radiationType,"expected",sep="_")
present         <- paste(c(radiationCs,radiationType),"present",sep="_")
future          <- paste(radiationCs,"expected",sep="_")
metadata        <- c("date","date_expected","wTypeF","zenit",future,"horizon")
exclude         <- c("date","date_expected","wTypeF","zenit",future)

#METODOS DE APRENDIZAJE ##############################################################################################################
methods         <- c("lm","xgbTree")

#NUMERO DE SEMANAS DE ENTRENAMIENTO y HORAS INCLUIDAS ################################################################################
daysOnlyTrain   <- 2 * 7
daysInValid     <- 1 * 7
daysInTrain     <- daysOnlyTrain + daysInValid
startHour       <- 9
endHour         <- 15
maxZenit        <- 75

#EXTRAER DATOS A PARTIR DE LOS PARÁMETROS ############################################################################################
all <- c(metadata,features,target)

#data <- sevilleData[(hour(date_expected) >= startHour) & (hour(date_expected) <= endHour), all, with= F]
data <- sevilleData[zenit < maxZenit, all, with= F]
#data[data==0] <- NA

#data[,avaiable:= data[,features,with=F][,lapply(.SD,complete.cases)][,rowSums(.SD)]]
#datStats <- data[,.(CIADCast= sum(complete.cases(ghi_CIADCast)),WRFSolar= sum(complete.cases(ghi_WRFsolar)), SmartPers= sum(complete.cases(ghi_SmartPersistence)),Satellite= sum(complete.cases(ghi_Satelite)), OneAv= sum(avaiable>=1),TwoAv= sum(avaiable>=2),ThreeAv= sum(avaiable>=3),AllAv=sum(avaiable==4)),by=list(hour(date_expected),horizon)]

if(useNA) {
    allPredictors <- data[,features,with=F][,lapply(.SD,complete.cases)][,rowSums(.SD)>0]
    allClasses <-    data[,target,with=F]  [,lapply(.SD,complete.cases)][,rowSums(.SD)>0]
    data <- data[allClasses & allPredictors & complete.cases(horizon)]
    data <- data[order(horizon,date)]
}else{
    data <-  data[complete.cases(data),]
} 

#importanceMatrix<- data.table(t(xtModel[,do.call(cbind,lapply(m,function(model){ 
#    imp <- varImp(model)$importance
#    names <- rownames(imp)[order(rownames(imp))]
#    orderedImp <- imp[names,]
#    names(orderedImp) <- names
#    orderedImp}))]))[,horizon:=seq(0,360,by=15)][,c(5,1:4),with=F]
#datStats <- data[,.(maxZenit= max(zenit),
#                    minZenit= min(zenit),
#                    meanRad=mean(ghi_expected,na.rm=T),
#                    maxRad= max(ghi_expected,na.rm=T),
#                    minRad= min(ghi_expected,na.rm=T),
#                    maxHour= max(hour(date_expected)),
#                    minHour= min(hour(date_expected)),
#                    maxStart= max(hour(date)),
#                    minStart= min(hour(date)),
#                    n= .N
#),by=horizon]

#NORMALIZAR LOS DATOS ################################################################################################################
norm <- function(feature,data,maxData,minData) (data[,feature,with = F] - minData[feature]) / (maxData[feature] - minData[feature])
dnrm <- function(v,min,max)  v * (max - min) + min

toNorm <- all[-c(1:length(exclude))]
maxData <- unlist(sevilleData[,toNorm,with=F][,lapply(.SD,max,na.rm=T)])
minData <- unlist(sevilleData[,toNorm,with=F][,lapply(.SD,min,na.rm=T)])

#maxData <- sevilleData[complete.cases(sevilleData),toNorm,with=F][,lapply(.SD,max,na.rm=T),by=horizon]
#minData <- sevilleData[complete.cases(sevilleData),toNorm,with=F][,lapply(.SD,min,na.rm=T),by=horizon]

normData <- foreach(i= toNorm,.combine= cbind) %do% {
    norm(i,data,maxData,minData)
}
normData <- cbind(normData,data[,-toNorm,with=F])
normData[,horizon:= data$horizon]

#ASIGNAR ENTRENAMIENTO, VALIDACION Y TEST ############################################################################################
inTrain      <- normData[, which(day(date_expected) <= (daysInTrain) )]
trainData    <- normData[ inTrain]
testData     <- normData[-inTrain]
inTrainValid <- trainData[,.(byLabel= which(day(date_expected) <= (daysOnlyTrain) )),by=horizon]

#FUNCIONES DE ERROR
RMSE  <- function(pred,obs) (sum((pred - obs) ^ 2) / length(obs)) ^ (1/2)
nRMSE <- function(pred,obs,mn) RMSE(pred,obs) / mn
MAE   <- function(pred,obs) sum(abs(pred - obs)) / length(obs)
R2    <- function(pred,obs) cor(pred,obs) ^ 2

N <- length(metadata)
meanTrain <- data[ inTrain,c(metadata,target),with=F][,.(horizon= .SD[[N]],target= .SD[[N+1]])][,.(mn= mean(target,na.rm=T)),by=horizon]
meanTest  <- data[-inTrain,c(metadata,target),with=F][,.(horizon= .SD[[N]],target= .SD[[N+1]])][,.(mn= mean(target,na.rm=T)),by=horizon]
if(useKt) {
    meanTrain <- data[ inTrain,c(metadata,target),with=F][,.(future= .SD[[N-1]], horizon= .SD[[N]],target= .SD[[N+1]])][,.(mn= mean(target*future,na.rm=T)),by=horizon]
    meanTest  <- data[-inTrain,c(metadata,target),with=F][,.(future= .SD[[N-1]], horizon= .SD[[N]],target= .SD[[N+1]])][,.(mn= mean(target*future,na.rm=T)),by=horizon]
}

#ENTRENAR TODOS LOS METODOS Y OBTENER ERRORES ########################################################################################
modelErrors <- data.table(foreach(i= methods,.combine=rbind) %do% {
    #ENTRENAMIENTO
    
    model <- trainData[,.(model= list(train( as.formula(paste(target,"~. -", paste(exclude,collapse=" - "),sep="")),
                                             .SD,
                                             method = i,
                                             tuneLegnth = 10,
                                             na.action = na.pass,
                                             trControl = trainControl(method = "LGOCV",index = inTrainValid[horizon == .BY[[1]],list(a=c(byLabel))] ,number = 1))
    )),
    by=horizon]
    
    colnames(model) <- c("s","m")
    
    #DESNORMALIZAR Y PREDECIR
    max <- maxData[target]
    min <- minData[target]

    csTr <- 1
    csTe <- 1
    
    if(useKt) csTr <- trainData[order(horizon,date)][,future,with=F]
    if(useKt) csTe <-  testData[order(horizon,date)][,future,with=F]
    
    trainPred <- dnrm(model[,predict(m,newdata = trainData[.BY[[1]] == horizon,],na.action= na.pass),by=s][,V1],min,max)
    testPred  <- dnrm(model[,predict(m,newdata =  testData[.BY[[1]] == horizon,],na.action= na.pass),by=s][,V1],min,max)
    trainObs  <- model[,trainData[.BY[[1]] == horizon,c(target,metadata),with=F],by=s][,obs:= dnrm(.SD[[2]],min,max)]
    testObs   <- model[, testData[.BY[[1]] == horizon,c(target,metadata),with=F],by=s][,obs:= dnrm(.SD[[2]],min,max)]
    
    trainErrorTable <- trainObs[,pred:= trainPred][order(horizon,date),][,-target,with=F]
    trainErrorTable[,pred:= pred*csTr][,obs:= obs*csTr]
    trainErrorTable[,s:=NULL]
    testErrorTable <- testObs[,pred:=  testPred][order(horizon,date),][,-target,with=F]
    testErrorTable[,pred:=  pred*csTe][,obs:= obs*csTe]
    testErrorTable[,s:=NULL]
    
    #DENORMALIZAR SALIDA ENTRENAMIENTO
    trainErrorTable[,type:="train"][,method:=i]
    testErrorTable[,type:="test"][,method:=i]

    #LISTA DE ERRORES
    
    predObs <- rbind(trainErrorTable,testErrorTable)
    
    list(predObs= predObs, model= model, method= i)
})

#SEPARAR PREDICCIONES Y MODELOS DE LA SALIDA #########################################################################################
predObs <- modelErrors[,do.call(rbind,as.list(predObs))]
models <- modelErrors[,.(method,model)]

#EXTRAER DATOS PARA COMPARAR #########################################################################################################
reference <- data.table(foreach(i=features,.combine=rbind) %do% {
    d <- data[-inTrain,c(metadata,i,target),with=F] # SOLO CUANDO PODEMOS DAR UNA PREDICCION
    colnames(d) <- c(metadata,"pred","obs")
    cs <- 1
    if(useKt){
        cs <- data[-inTrain,future,with=F]
    }
    d[ ,obs:=obs*cs]
    d[ ,pred:=pred*cs]
    d[,type:= "baseline"][,method:=gsub(paste(radiationType,"_",sep=""),"",i)]
    d
})

#CALCULAR ERRORES A PARTIR DE PREDICCIONES Y OBSERVACIONES
allPredObs <- rbind(predObs,reference)
completeDates   <- allPredObs[ method=="WRFsolar" & complete.cases(allPredObs),paste(date,horizon) ]
completePredObs <- allPredObs[ paste(date,horizon) %in% completeDates ]

allErrors <- allPredObs[complete.cases(allPredObs),.(
    RMSE=   RMSE(pred,obs),
    nRMSE= nRMSE(pred,obs,mn= meanTrain[horizon==.BY[[1]],mn]),
    MAE=     MAE(pred,obs),
    R2=       R2(pred,obs),
    n= .N
),by=list(horizon,type,method)]
allErrorsTimeType <- allPredObs[complete.cases(allPredObs),.(
    RMSE=   RMSE(pred,obs),
    nRMSE= nRMSE(pred,obs,mn= meanTrain[horizon==.BY[[1]],mn]),
    MAE=     MAE(pred,obs),
    R2=       R2(pred,obs),
    n= .N
),by=list(horizon,wTypeF,type,method)]
completeErrors <- completePredObs[method %in% c(methods,"WRFsolar","CIADCast") & complete.cases(completePredObs),.(
    RMSE=   RMSE(pred,obs),
    nRMSE= nRMSE(pred,obs,mn= meanTrain[horizon==.BY[[1]],mn]),
    MAE=     MAE(pred,obs),
    R2=       R2(pred,obs),
    n= .N
),by=list(horizon,type,method)]

#CALCULAR ERRORES POR HORIZONTE Y SUMARIO DE ERRORES #################################################################################
errorSummary <-         allErrors[order(type,decreasing= T),.(RMSE= mean(RMSE),nRMSE= mean(nRMSE),MAE= mean(MAE),R2= mean(R2)),by=c("method","type")]
errorSummaryComplete <- completeErrors[order(type,decreasing= T),.(RMSE= mean(RMSE),nRMSE= mean(nRMSE),MAE= mean(MAE),R2= mean(R2)),by=c("method","type")]

#SELECCIONAR NOMBRES DE FICHEROS #####################################################################################################
outResult  <- paste(RESDIR,paste(fileID,"csv",sep="."),sep="/")
outResult2 <- paste(RESDIR,paste(fileID,"summary.csv",sep="_"),sep="/")
outResult2Complete <- paste(RESDIR,paste(fileID,"complete_summary.csv",sep="_"),sep="/")
outResult3 <- paste(RESDIR,paste(fileID,"predObs.csv",sep="_"),sep="/")
outResult4 <- paste(RESDIR,paste(fileID,"timeType.csv",sep="_"),sep="/")
outPlot    <- paste(PLTDIR,paste(fileID,"plot.png",sep="_"),sep="/")
outPlotMae <- paste(PLTDIR,paste(fileID,"MAEplot.png",sep="_"),sep="/")
outPlotComplete    <- paste(PLTDIR,paste(fileID,"complete_plot.png",sep="_"),sep="/")
outPlotCompleteMae <- paste(PLTDIR,paste(fileID,"complete_MAEplot.png",sep="_"),sep="/")
outModels  <- paste(MODDIR,paste(fileID,"_",methods,".rds",sep=""),sep="/")

#GUARDAR RESULTADOS ##################################################################################################################
write.csv2(allErrors,outResult)
write.csv2(errorSummary,outResult2)
write.csv2(errorSummaryComplete,outResult2Complete)
write.csv2(allErrors,outResult)
write.csv2(allPredObs,outResult3)
write.csv2(allErrorsTimeType,outResult4)

#DIBUJAR LA GRÄFICA DE nRMSE #########################################################################################################
plot.new()
p <- ggplot(data= allErrors[type=="test" | type=="baseline"], aes(x=horizon,y=nRMSE,group=method,colour=method)) + geom_line(size=1) + geom_point(shape=21,size=3,fill="white") + scale_color_brewer(palette="Paired",type="div") + theme_light()
print(p)
dev.copy(png,outPlot)
dev.off()

#DIBUJAR LA GRÄFICA DE MAE   #########################################################################################################
plot.new()
p <- ggplot(data= allErrors[type=="test" | type=="baseline"], aes(x=horizon,y=MAE,group=method,colour=method)) + geom_line(size=1) + geom_point(shape=21,size=3,fill="white") + scale_color_brewer(palette="Paired",type="div") + theme_light()
print(p)
dev.copy(png,outPlotMae)
dev.off()

#DIBUJAR LA GRÄFICA DE nRMSE PARA CASOS COMPLETOS ####################################################################################
plot.new()
p <- ggplot(data= completeErrors[type=="test" | type=="baseline"], aes(x=horizon,y=nRMSE,group=method,colour=method)) + geom_line(size=1) + geom_point(shape=21,size=3,fill="white") + scale_color_brewer(palette="Paired",type="div") + theme_light()
print(p)
dev.copy(png,outPlotComplete)
dev.off()

#DIBUJAR LA GRÄFICA DE MAE PARA CASOS COMPLETOS ######################################################################################
plot.new()
p <- ggplot(data= completeErrors[type=="test" | type=="baseline"], aes(x=horizon,y=MAE,group=method,colour=method)) + geom_line(size=1) + geom_point(shape=21,size=3,fill="white") + scale_color_brewer(palette="Paired",type="div") + theme_light()
print(p)
dev.copy(png,outPlotCompleteMae)
dev.off()

#GUARDAR LOS MODELOS
foreach(i= methods) %do% {
    saveRDS(models[i==method,model],grep(i,outModels,value=T))
}
