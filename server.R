
#shiny::runApp('D:/SHINYPROJECT/StrategyAnalysis',host="192.168.115.234",port=3700)
# shiny::runApp(""D:/SHINYPROJECT/StrategyAnalysis",display.mode="showcase")
# options(shiny.reactlog=TRUE) 
#require("shinyapps")
# shinyapps::deployApp("D:/SHINYPROJECT/StrategyAnalysis", account='prashantapp')
#setwd("D:/SHINYPROJECT/StrategyAnalysis")
zz <- file("C:/DeployApp/error.txt", open="wt")
sink(zz, type="message")
system.time<- Sys.time()
print(paste("Loading pakages..", system.time))
library(shiny)
library(lubridate)
library(data.table)
#library(RMySQL)
library(quantmod)
library(PerformanceAnalytics)
library(ggplot2)
library(reshape2)
library(dygraphs)
library(corrplot)
library(dplyr)
library(psych)
library(pracma)
library(urca)
library(latticeExtra)
library(XML)
library(doParallel)#library(foreach)
library(doSNOW)
library(foreach)
library(RODBC) 
#library(DT)
source('./VerticalAndHorizontalAnalysis.R')
#library(fUnitRoots)
#library(googleVis)
print(paste("Pakages loaded..", "Took ", Sys.time()-system.time))
options(digits=10)
FinalPortfolio<-NULL
data<-NULL
gc()
createMasterTable <- function(startDate, endDate, inst1, ratio1, inst2, ratio2, inst3, ratio3, inst4, ratio4, inst5, ratio5,  inst6, inst7, inst8, inst9, inst10, nInterval, Interval, indexOrBond, sma, database, multiplier, minbp, maxbp){
 startDate<-'2016-01-01'
 endDate<-'2017-07-17'
 inst1<-"AD"
 ratio1<- 588
 inst2<-"TY"
 ratio2<- -12
 inst3<-"AX"
 ratio3<- 100
 inst4<-""
 ratio4<-1
 inst5<-""
 ratio5<-1
 inst6<-"UD"
 inst7<-"UD"
 inst8<-"UD"
 inst9<-"UD"
 inst10<-"UD"
 nInterval<-240
 Interval<-"mins"
 indexOrBond<-T
 database="Rolled Price1"
 sma=200
 multiplier = 1
 minbp=2
 maxbp=12
 database="Original Price"

  system.time<- Sys.time()
  print(paste("Fetching Data", system.time))

  sqlData <-NULL
  nInst<-0
  data1<-NULL
  data2<-NULL
  data3<-NULL
  data4<-NULL
  data5<-NULL
  data6<-NULL
  data7<-NULL
  data8<-NULL
  data9<-NULL
  data10<-NULL
  curr6Selected<-FALSE
  curr7Selected<-FALSE
  curr8Selected<-FALSE
  curr9Selected<-FALSE
  curr10Selected<-FALSE
  #con <- dbConnect(MySQL(), user='root', password='data786', dbname='FlatRolls_Data', host='192.168.115.233')
  #con <- dbConnect(MySQL(), user='external', password='data786', dbname='pricedata', host='49.128.39.130')
  #con <- dbConnect(MySQL(), user='root', password='data786', dbname='GMT_Minute_Data', host='192.168.115.233')
  #con <- dbConnect(MySQL(), user='external', password='External786', dbname='FlatRolls_Data', host='192.168.115.233', port=3306)
  #con <- dbConnect(MySQL(), user='external', password='External786', dbname='FlatRolls_Data', host='49.128.39.128', port=31313)
  con<-NULL
  #con <- dbConnect(MySQL(), user='external', password='External786', dbname='FlatRolls_Data', host='0.0.0.0')
#   if(database=="Original Price"){
#     con <- dbConnect(MySQL(), user='external', password='data786', dbname='GMT_Minute_Data', host='49.128.39.130')
#   } else {
#     con <- dbConnect(MySQL(), user='external', password='data786', dbname='FlatRolls_Data', host='49.128.39.130')
#   }
doc <- xmlParse("C:/DeployApp/ipconfig.xml")
useinternal<-xmlAttrs(xmlRoot(doc)[["useInternal"]])[[1]]
ip = ifelse(useinternal=="True",
            xmlAttrs(xmlRoot(doc)[["internalIP"]]),
            xmlAttrs(xmlRoot(doc)[["externalIP"]])
)
print(paste("IP: ", ip, getwd()))
#192.168.115.233
  #if(database=="Original Price"){
  #  con <- dbConnect(MySQL(), user='root', password='data786', dbname='GMT_Minute_Data', host=ip)
  #} else {
  #  con <- dbConnect(MySQL(), user='root', password='data786', dbname='FlatRolls_Data', host=ip)
  #}
#
#con <- dbConnect(MySQL(), user='root', password='external', dbname='Edo', host='49.128.39.130', port =3306)
#sqlData <- dbSendQuery(con, paste0('select * from test'))
#data1 = dbFetch(sqlData, n=-1);data1

dbhandle <- odbcConnect("SQL_DB_Dev")

nInst=0
  if(inst1 != ""){
    data1 <- as.data.table(sqlQuery(dbhandle, paste0('select DtTime, Open_V, High_V, Low_V, Close_V from [qmTIMESERIES_DB].[dbo].[IDContinuousTSBars] where ContinuousName =', "'", inst1, "'", ' and DtTime between \'',  startDate, '\' and \'',  endDate, '\'' )))
   # sqlData <- dbSendQuery(con, paste0('select TradeTime, price from ad'))
   # data1 = dbFetch(sqlData, n=-1)
    nInst<-nInst+1
  } 
  if(inst2 != ""){
    data2 <- as.data.table(sqlQuery(dbhandle, paste0('select DtTime, Open_V, High_V, Low_V, Close_V from [qmTIMESERIES_DB].[dbo].[IDContinuousTSBars] where ContinuousName =', "'", inst2, "'", ' and DtTime between \'',  startDate, '\' and \'',  endDate, '\'' )))
    #data2 = dbFetch(sqlData, n=-1)
    nInst<-nInst+1
  }
  if(inst3 != ""){
    data3 <- as.data.table(sqlQuery(dbhandle, paste0('select DtTime, Open_V, High_V, Low_V, Close_V from [qmTIMESERIES_DB].[dbo].[IDContinuousTSBars] where ContinuousName =', "'", inst3, "'", ' and DtTime between \'',  startDate, '\' and \'',  endDate, '\'' )))
    #data3 = dbFetch(sqlData, n=-1)
    nInst<-nInst+1
  }
  if(inst4 != ""){
    data4 <- as.data.table(sqlQuery(dbhandle, paste0('select DtTime, Open_V, High_V, Low_V, Close_V from [qmTIMESERIES_DB].[dbo].[IDContinuousTSBars] where ContinuousName =', "'", inst4, "'", ' and DtTime between \'',  startDate, '\' and \'',  endDate, '\'' )))
    #data4 = dbFetch(sqlData, n=-1)
    nInst<-nInst+1
  }
  if(inst5 != ""){
    data5 <- as.data.table(sqlQuery(dbhandle, paste0('select DtTime, Open_V, High_V, Low_V, Close_V from [qmTIMESERIES_DB].[dbo].[IDContinuousTSBars] where ContinuousName =', "'", inst5, "'", ' and DtTime between \'',  startDate, '\' and \'',  endDate, '\'' )))
    #data5 = dbFetch(sqlData, n=-1)
    nInst<-nInst+1
  }
  if(inst6 != "UD"){
    data6 <- as.data.table(sqlQuery(dbhandle, paste0('select DtTime, Open_V, High_V, Low_V, Close_V from [qmTIMESERIES_DB].[dbo].[IDContinuousTSBars] where ContinuousName =', "'", inst6, "'", ' and DtTime between \'',  startDate, '\' and \'',  endDate, '\'' )))
    #data6 = dbFetch(sqlData, n=-1)
    curr6Selected<-TRUE
  }
  if(inst7 != "UD"){
    data7 <- as.data.table(sqlQuery(dbhandle, paste0('select DtTime, Open_V, High_V, Low_V, Close_V from [qmTIMESERIES_DB].[dbo].[IDContinuousTSBars] where ContinuousName =', "'", inst7, "'", ' and DtTime between \'',  startDate, '\' and \'',  endDate, '\'' )))
    #data7 = dbFetch(sqlData, n=-1)
    curr7Selected<-TRUE
  }
  if(inst8 != "UD"){
    data8 <- as.data.table(sqlQuery(dbhandle, paste0('select DtTime, Open_V, High_V, Low_V, Close_V from [qmTIMESERIES_DB].[dbo].[IDContinuousTSBars] where ContinuousName =', "'", inst8, "'", ' and DtTime between \'',  startDate, '\' and \'',  endDate, '\'' )))
    #data8 = dbFetch(sqlData, n=-1)
    curr8Selected<-TRUE
  }
  if(inst9 != "UD"){
    data9 <- as.data.table(sqlQuery(dbhandle, paste0('select DtTime, Open_V, High_V, Low_V, Close_V from [qmTIMESERIES_DB].[dbo].[IDContinuousTSBars] where ContinuousName =', "'", inst9, "'", ' and DtTime between \'',  startDate, '\' and \'',  endDate, '\'' )))
    #data9 = dbFetch(sqlData, n=-1)
    curr9Selected<-TRUE
  }
  if(inst10 != "UD"){
    data10 <- as.data.table(sqlQuery(dbhandle, paste0('select DtTime, Open_V, High_V, Low_V, Close_V from [qmTIMESERIES_DB].[dbo].[IDContinuousTSBars] where ContinuousName =', "'", inst10, "'", ' and DtTime between \'',  startDate, '\' and \'',  endDate, '\'' )))
    #data10 = dbFetch(sqlData, n=-1)
    curr10Selected<-TRUE
  }
  odbcCloseAll()
  #connections <- dbListConnections(MySQL())
  #for(i in connections) {dbDisconnect(i)}
  print(paste("Data Fetched", "Took ", Sys.time()-system.time))
  print(head(data1))
  print(nrow(data1))
  print(head(data2))
  print(nrow(data2))
  print(head(data3))
  print(nrow(data3))
  print(head(data4))
  print(nrow(data4))
  system.time<- Sys.time()
  print(paste("Merging Data", system.time))
  incProgress(1/4)
  #suppressWarnings(dbDisconnect(con))
  if(nInst==1)
    stop("Please select more than one instruments.")
  if(nInst >= 1){
    #xtsData1 <- as.xts(data1[2:5],  ymd_hms(data1[,1], tz="GMT"))
    xtsData1 <- as.xts(as.data.frame(data1[,.(Open_V, High_V, Low_V, Close_V)]),  ymd_hms(data1[,DtTime]))
    indexTZ(xtsData1)<-"Asia/Kuala_Lumpur"
  }
  if(nInst >= 2){
    #xtsData2 <- as.xts(data2[2:5],  ymd_hms(data2[,1], tz="GMT"))
    xtsData2 <- as.xts(as.data.frame(data2[,.(Open_V, High_V, Low_V, Close_V)]),  ymd_hms(data2[,DtTime]))
    indexTZ(xtsData1)<-"Asia/Kuala_Lumpur"
  }
  if(nInst >= 3){
    xtsData3 <- as.xts(as.data.frame(data3[,.(Open_V, High_V, Low_V, Close_V)]),  ymd_hms(data3[,DtTime]))
    indexTZ(xtsData1)<-"Asia/Kuala_Lumpur"
  }
  if(nInst >= 4){
    xtsData4 <- as.xts(as.data.frame(data4[,.(Open_V, High_V, Low_V, Close_V)]),  ymd_hms(data4[,DtTime]))
    indexTZ(xtsData1)<-"Asia/Kuala_Lumpur"
  }
  if(nInst == 5){
    xtsData5 <- as.xts(as.data.frame(data5[,.(Open_V, High_V, Low_V, Close_V)]),  ymd_hms(data5[,DtTime]))
    indexTZ(xtsData1)<-"Asia/Kuala_Lumpur"
  }
  if(curr6Selected){
    xtsData6 <- as.xts(data6[2:5],  ymd_hms(data6[,1], tz="GMT"))
    #indexTZ(xtsData1)<-"Asia/Kuala_Lumpur"
  }
  if(curr7Selected){
    xtsData7 <- as.xts(data7[2:5],  ymd_hms(data7[,1], tz="GMT"))
    #indexTZ(xtsData1)<-"Asia/Kuala_Lumpur"
  }
  if(curr8Selected){
    xtsData8 <- as.xts(data8[2:5],  ymd_hms(data8[,1], tz="GMT"))
    #indexTZ(xtsData1)<-"Asia/Kuala_Lumpur"
  }
  if(curr9Selected){
    xtsData9 <- as.xts(data9[2:5],  ymd_hms(data9[,1], tz="GMT"))
    #indexTZ(xtsData1)<-"Asia/Kuala_Lumpur"
  }
  if(curr10Selected){
    xtsData10 <- as.xts(data10[2:5],  ymd_hms(data10[,1], tz="GMT"))
    #indexTZ(xtsData1)<-"Asia/Kuala_Lumpur"
  }

#   print(head(xtsData1))
#   print(head(xtsData2))
#   print(head(xtsData3))
    
  rm(data1)
  rm(data2)
  rm(data3)
  rm(data4)
  rm(data5)
  rm(data6)
  rm(data7)
  rm(data8)
  rm(data9)
  rm(data10)
  
  ################ Merge all products in minute data
  finalMergedData<-NULL
  #do.call(merge, R)  do.call(merge, monthly)
  #cdates <- Reduce(intersect,list(data1$TradeTime,data2$TradeTime,data3$TradeTime))
  # if(nInst >= 2){
  #   finalMergedData<-na.omit(merge(xtsData1,xtsData2))
  # }
  # if(nInst >= 3){
  #   finalMergedData<-na.omit(merge(finalMergedData, xtsData3))
  # }
  # if(nInst >= 4)
  #   finalMergedData<-na.omit(merge(finalMergedData, xtsData4))
  # if(nInst == 5)
  #   finalMergedData<-na.omit(merge(finalMergedData, xtsData5))
  # if(curr6Selected)
  #   finalMergedData<-na.omit(merge(finalMergedData, xtsData6))
  # if(curr7Selected)
  #   finalMergedData<-na.omit(merge(finalMergedData, xtsData7))
  # if(curr8Selected)
  #   finalMergedData<-na.omit(merge(finalMergedData, xtsData8))
  # if(curr9Selected)
  #   finalMergedData<-na.omit(merge(finalMergedData, xtsData9))
  # if(curr10Selected)
  #   finalMergedData<-na.omit(merge(finalMergedData, xtsData10))
  
  if(nInst >= 2){
    finalMergedData1<-merge(xtsData1,xtsData2, join="outer")
    finalMergedData <- na.omit(na.locf(finalMergedData1))
    print(nrow(finalMergedData))
  }
  if(nInst >= 3){
    finalMergedData2<-merge(finalMergedData,xtsData3, join="outer")
    finalMergedData <- na.omit(na.locf(finalMergedData2))
    print(nrow(finalMergedData))
  }
  if(nInst >= 4)
  {
    finalMergedData3<-merge(finalMergedData,xtsData4, join="outer")
    finalMergedData <- na.omit(na.locf(finalMergedData3))
    print(nrow(finalMergedData))
  }
  if(nInst == 5)
  {
    finalMergedData4<-na.omit(merge(finalMergedData, xtsData5), join="outer")
    finalMergedData <- na.omit(na.locf(finalMergedData4))
    print(nrow(finalMergedData))
  }
  if(curr6Selected){
    finalMergedData5<-na.omit(merge(finalMergedData, xtsData6), join="outer")
    finalMergedData <- na.omit(na.locf(finalMergedData5))
    print(nrow(finalMergedData))
  }
  if(curr7Selected){
    finalMergedData6<-na.omit(merge(finalMergedData, xtsData7), join="outer")
    finalMergedData <- na.omit(na.locf(finalMergedData6))
    print(nrow(finalMergedData))
  }
  if(curr8Selected){
    finalMergedData7<-na.omit(merge(finalMergedData, xtsData8), join="outer")
    finalMergedData <- na.omit(na.locf(finalMergedData7))
    print(nrow(finalMergedData))
  }
  if(curr9Selected){
    finalMergedData8<-na.omit(merge(finalMergedData, xtsData9), join="outer")
    finalMergedData <- na.omit(na.locf(finalMergedData8))
    print(nrow(finalMergedData))
  }
  if(curr10Selected){
    finalMergedData<-na.omit(merge(finalMergedData, xtsData10), join="outer")
    finalMergedData <- na.omit(na.locf(finalMergedData3))
    print(nrow(finalMergedData))
  }
  
  print(paste("Merged..", "Took ", Sys.time()-system.time))
#   print(head(finalMergedData))
 
  #write.csv(data.frame(xtsData1), "xtsData1.csv")
  #write.csv(data.frame(xtsData2), "xtsData2.csv")
  #write.csv(data.frame(xtsData3), "xtsData3.csv")
  #write.csv(data.frame(xtsData4), "xtsData4.csv")

  #write.csv(data.frame(finalMergedData), "Me rgedSeries.csv")

  xtsData1<-NULL
  xtsData2<-NULL
  xtsData3<-NULL
  xtsData4<-NULL
  xtsData5<-NULL
  xtsData6<-NULL
  xtsData7<-NULL
  xtsData8<-NULL
  xtsData9<-NULL
  xtsData10<-NULL
  finalMergedData1<-NULL
  finalMergedData2<-NULL
  finalMergedData3<-NULL
  finalMergedData4<-NULL
  finalMergedData5<-NULL
  finalMergedData6<-NULL
  finalMergedData7<-NULL
  finalMergedData8<-NULL
  
  #to.daily not working on all columns therefore seperating all inst and aggregating individually
  varCurrencyColumn=4
  offset=(nInst * 4) 
  if(nInst >= 1)
    xtsData1 <- finalMergedData[,c(1:4)]
  if(nInst >= 2)
    xtsData2 <- finalMergedData[,c(5:8)]
  if(nInst >= 3)
    xtsData3 <- finalMergedData[,c(9:12)]
  if(nInst >= 4)
    xtsData4 <- finalMergedData[,c(13:16)]
  if(nInst == 5)
    xtsData5 <- finalMergedData[,c(17:20)]
  if(curr6Selected){
    xtsData6 <- finalMergedData[,c((offset + 1):(offset + varCurrencyColumn))]
    offset=offset+4
  }
  if(curr7Selected){
    xtsData7 <- finalMergedData[,c((offset + 1):(offset + varCurrencyColumn))]
    offset=offset+4
  }
  if(curr8Selected){
    xtsData8 <- finalMergedData[,c((offset + 1):(offset + varCurrencyColumn))]
    offset=offset+4
  }
  if(curr9Selected){
    xtsData9 <- finalMergedData[,c((offset + 1):(offset + varCurrencyColumn))]
    offset=offset+4
  }
  if(curr10Selected){
    xtsData10 <- finalMergedData[,c((offset + 1):(offset + varCurrencyColumn))]
  }
# 
# print(head(xtsData1))
# write.csv(as.data.frame(xtsData1), "D:/SHINYPROJECT/StrategyAnalysis/Merged.csv",row.names = T)
# xtsDatah1<-to.period(xtsData1,"hours", 1, indexAt="startof", OHLC=T)
# print(head(xtsDatah1))
# write.csv(as.data.frame(xtsDatah1), "D:/SHINYPROJECT/StrategyAnalysis/MergedH1.csv",row.names = T)
# 
# xtsDatah<-to.period(xtsData1,"mins",24*60 , indexAt="startof", OHLC=T)
# xtsDatah[!duplicated(strptime(index(xtsDatah),"%F"), fromLast=T),]
# print(head(xtsDatah))
# write.csv(as.data.frame(xtsDatah), "D:/SHINYPROJECT/StrategyAnalysis/MergedWithHourly.csv",row.names = T)
# xtsDatad<-to.daily(xtsData1,drop.time=F, 1, indexAt="startof", OHLC=T)
# print(head(xtsDatad))
# write.csv(as.data.frame(xtsDatad), "D:/SHINYPROJECT/StrategyAnalysis/Daily.csv",row.names = T)

  ################################################### Aggregate into higher time Frame
  incProgress(1/4)
  system.time<- Sys.time()
  print(paste("Converting timeframe and merging again", system.time))

#period.apply(x, endpoints(x,"hours"), sum) 
  #print("Converting Time Frame")

if(Interval=="mins"){
  if(nInst >= 1)
    xtsData1<-to.period(xtsData1,"minutes", nInterval, indexAt="startof", OHLC=T)
  if(nInst >= 2)
    xtsData2<-to.period(xtsData2,"minutes", nInterval, indexAt="startof", OHLC=T)
  if(nInst >= 3)
    xtsData3<-to.period(xtsData3,"minutes", nInterval, indexAt="startof", OHLC=T)
  if(nInst >= 4)
    xtsData4<-to.period(xtsData4,"minutes", nInterval, indexAt="startof", OHLC=T)
  if(nInst >= 5)
    xtsData5<-to.period(xtsData5,"minutes", nInterval, indexAt="startof", OHLC=T)
  if(!is.null(xtsData6))
    xtsData6<-to.period(xtsData6,"minutes", nInterval, indexAt="startof", OHLC=T)
  if(!is.null(xtsData7))
    xtsData7<-to.period(xtsData7,"minutes", nInterval, indexAt="startof", OHLC=T)
  if(!is.null(xtsData8))
    xtsData8<-to.period(xtsData8,"minutes", nInterval, indexAt="startof", OHLC=T)
  if(!is.null(xtsData9))
    xtsData9<-to.period(xtsData9,"minutes", nInterval, indexAt="startof", OHLC=T)
  if(!is.null(xtsData10))
    xtsData10<-to.period(xtsData10,"minutes", nInterval, indexAt="startof", OHLC=T)
  
} else if(Interval=="hours")  {
  if(nInst >= 1)
    xtsData1<-to.period(xtsData1,"hours", nInterval, indexAt="startof", OHLC=T)
  if(nInst >= 2)
    xtsData2<-to.period(xtsData2,"hours", nInterval, indexAt="startof", OHLC=T)
  if(nInst >= 3)
    xtsData3<-to.period(xtsData3,"hours", nInterval, indexAt="startof", OHLC=T)
  if(nInst >= 4)
    xtsData4<-to.period(xtsData4,"hours", nInterval, indexAt="startof", OHLC=T)
  if(nInst >= 5)
    xtsData5<-to.period(xtsData5,"hours", nInterval, indexAt="startof", OHLC=T)
  if(!is.null(xtsData6))
    xtsData6<-to.period(xtsData6,"hours", nInterval, indexAt="startof", OHLC=T)
  if(!is.null(xtsData7))
    xtsData7<-to.period(xtsData7,"hours", nInterval, indexAt="startof", OHLC=T)
  if(!is.null(xtsData8))
    xtsData8<-to.period(xtsData8,"hours", nInterval, indexAt="startof", OHLC=T)
  if(!is.null(xtsData9))
    xtsData9<-to.period(xtsData9,"hours", nInterval, indexAt="startof", OHLC=T)
  if(!is.null(xtsData10))
    xtsData10<-to.period(xtsData10,"hours", nInterval, indexAt="startof", OHLC=T)
  
} else if(Interval=="daily")  {
  if(nInst >= 1){
    #xtsData1<-to.daily(xtsData1,drop.time=T, nInterval, indexAt="startof", OHLC=T)
    xtsData1<-to.period(xtsData1,"mins",24*60 , indexAt="startof", OHLC=T)
    xtsData1<-xtsData1[!duplicated(strptime(index(xtsData1),"%F"), fromLast=T),]
  }
  if(nInst >= 2){
    xtsData2<-to.period(xtsData2,"mins",24*60 , indexAt="startof", OHLC=T)
    xtsData2<-xtsData2[!duplicated(strptime(index(xtsData2),"%F"), fromLast=T),]
  }
  if(nInst >= 3){
    xtsData3<-to.period(xtsData3,"mins",24*60 , indexAt="startof", OHLC=T)
    xtsData3<-xtsData3[!duplicated(strptime(index(xtsData3),"%F"), fromLast=T),]
  }
  if(nInst >= 4){
    xtsData4<-to.period(xtsData4,"mins",24*60 , indexAt="startof", OHLC=T)
    xtsData4<-xtsData4[!duplicated(strptime(index(xtsData4),"%F"), fromLast=T),]
  }
  if(nInst >= 5){
    xtsData5<-to.period(xtsData5,"mins",24*60 , indexAt="startof", OHLC=T)
    xtsData5<-xtsData6[!duplicated(strptime(index(xtsData5),"%F"), fromLast=T),]
  }
  if(!is.null(xtsData6)){
    xtsData6<-to.period(xtsData6,"mins",24*60 , indexAt="startof", OHLC=T)
    xtsData6<-xtsData6[!duplicated(strptime(index(xtsData6),"%F"), fromLast=T),]
  }
  if(!is.null(xtsData7)){
    xtsData7<-to.period(xtsData7,"mins",24*60 , indexAt="startof", OHLC=T)
    xtsData7<-xtsData7[!duplicated(strptime(index(xtsData7),"%F"), fromLast=T),]
  }
  if(!is.null(xtsData8)){
    xtsData8<-to.period(xtsData8,"mins",24*60 , indexAt="startof", OHLC=T)
    xtsData8<-xtsData8[!duplicated(strptime(index(xtsData8),"%F"), fromLast=T),]
  }
  if(!is.null(xtsData9)){
    xtsData9<-to.period(xtsData9,"mins",24*60 , indexAt="startof", OHLC=T)
    xtsData9<-xtsData9[!duplicated(strptime(index(xtsData9),"%F"), fromLast=T),]
  }
  if(!is.null(xtsData10)){
    xtsData10<-to.period(xtsData10,"mins",24*60 , indexAt="startof", OHLC=T)
    xtsData10<-xtsData10[!duplicated(strptime(index(xtsData10),"%F"), fromLast=T),]
  }
  
} else if(Interval=="weekly") {
  if(nInst >= 1)
    xtsData1<-to.weekly(xtsData1,drop.time=T, nInterval, indexAt="startof", OHLC=T)
  if(nInst >= 2)
    xtsData2<-to.weekly(xtsData2,drop.time=T, nInterval, indexAt="startof", OHLC=T)
  if(nInst >= 3)
    xtsData3<-to.weekly(xtsData3,drop.time=T, nInterval, indexAt="startof", OHLC=T)
  if(nInst >= 4)
    xtsData4<-to.weekly(xtsData4,drop.time=T, nInterval, indexAt="startof", OHLC=T)
  if(nInst >= 5)
    xtsData5<-to.weekly(xtsData5,drop.time=T, nInterval, indexAt="startof", OHLC=T)
  if(!is.null(xtsData6))
    xtsData6<-to.weekly(xtsData6,drop.time=T, nInterval, indexAt="startof", OHLC=T)
  if(!is.null(xtsData7))
    xtsData7<-to.weekly(xtsData7,drop.time=T, nInterval, indexAt="startof", OHLC=T)
  if(!is.null(xtsData8))
    xtsData8<-to.weekly(xtsData8,drop.time=T, nInterval, indexAt="startof", OHLC=T)
  if(!is.null(xtsData9))
    xtsData9<-to.weekly(xtsData9,drop.time=T, nInterval, indexAt="startof", OHLC=T)
  if(!is.null(xtsData10))
    xtsData10<-to.weekly(xtsData10,drop.time=T, nInterval, indexAt="startof", OHLC=T)
  }
 

  Currency.Close<-NULL
  if(nInst >= 2){
    finalMergedData<-na.omit(merge(xtsData1,xtsData2))
    unitData<-rep(1, length(index(finalMergedData)))
    Currency.Close<-as.xts(unitData, index(finalMergedData))
  }
  if(nInst >= 3){
    finalMergedData<-na.omit(merge(finalMergedData, xtsData3))
  }
  if(nInst >= 4)
    finalMergedData<-na.omit(merge(finalMergedData, xtsData4))
  if(nInst == 5)
    finalMergedData<-na.omit(merge(finalMergedData, xtsData5))
  if(!is.null(xtsData6)){
    finalMergedData<-na.omit(merge(finalMergedData, xtsData6))
  } else{
    finalMergedData<-na.omit(merge(finalMergedData, Currency.Close))
  }
  if(!is.null(xtsData7)){
    finalMergedData<-na.omit(merge(finalMergedData, xtsData7))
  } else{
    finalMergedData<-na.omit(merge(finalMergedData, Currency.Close))
  }
  if(!is.null(xtsData8)){
    finalMergedData<-na.omit(merge(finalMergedData, xtsData8))
  } else{
    finalMergedData<-na.omit(merge(finalMergedData, Currency.Close))
  }
  if(!is.null(xtsData9)){
    finalMergedData<-na.omit(merge(finalMergedData, xtsData9))
  } else{
    finalMergedData<-na.omit(merge(finalMergedData, Currency.Close))
  }
  if(!is.null(xtsData10)){
    finalMergedData<-na.omit(merge(finalMergedData, xtsData10))
  } else{
    finalMergedData<-na.omit(merge(finalMergedData, Currency.Close))
  }
  write.csv(data.frame(finalMergedData), "D:/SHINYPROJECT/StrategyAnalysis/MergedSeriesWithTimeConversion.csv")
  
  print(paste("Finished converting and merging", "Took ", Sys.time()-system.time))

  ColList<-grep(c("Close"), names(finalMergedData), value=T)
#   ColList2<-grep(c("Currency"), names(finalMergedData), value=T)
#   ColList<-c(ColList1,ColList2)
  nCol<-length(ColList)
  print(paste("ColList", ColList))
  FinalPortfolio<-NULL
  FinalPortfolio <- data.table(TradeTime=index(finalMergedData), coredata(finalMergedData)[,ColList])
  
  rm(finalMergedData)
  rm(xtsData1)
  rm(xtsData2)
  rm(xtsData3)
  rm(xtsData4)
  rm(xtsData5)
  rm(xtsData6)
  rm(xtsData7)
  rm(xtsData8)
  rm(xtsData9)
  rm(xtsData10)
  ############################################################  Create Strategy
  incProgress(1/4)
  system.time<- Sys.time()
  print(paste("Calculating returns",system.time))
  #x<-c(1,2,8,4,5)
  logreturn1 <- function(x) log(tail(x,-1)/head(x,-1))  #MAKE SURE PRIC SERICE IN ASSENDING ORDER ELSE RETULE WILL BE -VE
  #c(0,diff(log(x))
  #Delt(x) % RETURNS ROC all givs the same result as diff(log(data)). They use exponential log excel use base 10 log
  # and multipling by currency change the log value and so does the correlation

  if(indexOrBond=="Index Model"){
    if(nInst==2){
      #print("2")
      pointValueInst1<-as.numeric(getFullPointValue(inst1))
      pointValueInst2<-as.numeric(getFullPointValue(inst2))
      formula=paste0("(",as.numeric(ratio1), " * ", ColList[1], " * ", pointValueInst1 , " * ", ColList[nCol-4],")", "(",as.numeric(ratio2), " * ", ColList[2], " * ", pointValueInst2 , " * ", ColList[nCol-3],")")
      print(formula)
      FinalPortfolio[, Spread:= (get(ColList[1]) * as.numeric(ratio1) * pointValueInst1 * get(ColList[nCol-4])) + (get(ColList[2]) *  as.numeric(ratio2) * pointValueInst2  * get(ColList[nCol-3]))]
      FinalPortfolio[, c("SpreadRet", inst1, inst2) := list(c(0,diff((Spread))), c(0,diff(log(get(ColList[1]) * get(ColList[nCol-4])))), c(0,diff(log(get(ColList[2]) * get(ColList[nCol-3])))))]
    }
    if(nInst==3){
      #FinalPortfolio[, Spread:= (AD/.21) - (Close.1 * .12) - (Close.19 * 100)]
      #print("3")
      pointValueInst1<-as.numeric(getFullPointValue(inst1))
      pointValueInst2<-as.numeric(getFullPointValue(inst2))
      pointValueInst3<-as.numeric(getFullPointValue(inst3))
      formula=paste0("(",as.numeric(ratio1), " * ", ColList[1], " * ", pointValueInst1 , " * ", ColList[nCol-4],")", "(",as.numeric(ratio2), " * ", ColList[2], " * ", pointValueInst2 , " * ", ColList[nCol-3],")", "(",as.numeric(ratio3), " * ", ColList[3], " * ", pointValueInst3 , " * ", ColList[nCol-2],")")
      print(formula)
      FinalPortfolio[, Spread:= (get(ColList[1]) *  as.numeric(ratio1) * pointValueInst1 * get(ColList[nCol-4])) + (get(ColList[2]) *  as.numeric(ratio2) * pointValueInst2 * get(ColList[nCol-3])) + (get(ColList[3]) *  as.numeric(ratio3) * pointValueInst3 * get(ColList[nCol-2]))]
      FinalPortfolio[, c("SpreadRet", inst1, inst2, inst3) := list(c(0,diff((Spread))), c(0,diff(log(get(ColList[1]) * get(ColList[nCol-4])))), c(0,diff(log(get(ColList[2]) * get(ColList[nCol-3])))), c(0,logreturn1(get(ColList[3]) * get(ColList[nCol-2]))))]
    }
    if(nInst==4){
      pointValueInst1<-as.numeric(getFullPointValue(inst1))
      pointValueInst2<-as.numeric(getFullPointValue(inst2))
      pointValueInst3<-as.numeric(getFullPointValue(inst3))
      pointValueInst4<-as.numeric(getFullPointValue(inst4))
      formula=paste0("(",as.numeric(ratio1), " * ", ColList[1], " * ", pointValueInst1 , " * ", ColList[nCol-4],")", "(",as.numeric(ratio2), " * ", ColList[2], " * ", pointValueInst2 , " * ", ColList[nCol-3],")", "(",as.numeric(ratio3), " * ", ColList[3], " * ", pointValueInst3 , " * ", ColList[nCol-2],") ",  " (",as.numeric(ratio4), " * ", ColList[4], " * ", pointValueInst4 , " * ", ColList[nCol-1],")")
      print(formula)
      FinalPortfolio[, Spread:= (get(ColList[1]) *  as.numeric(ratio1) * pointValueInst1 * get(ColList[nCol-4])) + (get(ColList[2]) *  as.numeric(ratio2) * pointValueInst2 * get(ColList[nCol-3])) + (get(ColList[3]) *  as.numeric(ratio3) * pointValueInst3 * get(ColList[nCol-2])) + (get(ColList[4]) *  as.numeric(ratio4) * pointValueInst4 * get(ColList[nCol-1]))]
      FinalPortfolio[, c("SpreadRet", inst1, inst2, inst3, inst4) := list(c(0,diff((Spread))), c(0,diff(log(get(ColList[1]) * get(ColList[nCol-4])))), c(0,diff(log(get(ColList[2]) * get(ColList[nCol-3])))), c(0,diff(log(get(ColList[3]) * get(ColList[nCol-2])))), c(0,logreturn1(get(ColList[4]) * get(ColList[nCol-1]))))]
    }
    if(nInst==5){
      pointValueInst1<-as.numeric(getFullPointValue(inst1))
      pointValueInst2<-as.numeric(getFullPointValue(inst2))
      pointValueInst3<-as.numeric(getFullPointValue(inst3))
      pointValueInst4<-as.numeric(getFullPointValue(inst4))
      pointValueInst5<-as.numeric(getFullPointValue(inst5))
      formula=paste0("(",as.numeric(ratio1), " * ", ColList[1], " * ", pointValueInst1 , " * ", ColList[nCol-4],")", "(",as.numeric(ratio2), " * ", ColList[2], " * ", pointValueInst2 , " * ", ColList[nCol-3],") ", " (",as.numeric(ratio3), " * ", ColList[3], " * ", pointValueInst3 , " * ", ColList[nCol-2],") ",  " (",as.numeric(ratio4), " * ", ColList[4], " * ", pointValueInst4 , " * ", ColList[nCol-1],")",  " (",as.numeric(ratio5), " * ", ColList[5], " * ", pointValueInst5 , " * ", ColList[nCol],")")
      print(formula)
      FinalPortfolio[, Spread:= (get(ColList[1]) *  as.numeric(ratio1) * pointValueInst1 * get(ColList[nCol-4])) + (get(ColList[2]) *  as.numeric(ratio2) * pointValueInst2 * get(ColList[nCol-3])) + (get(ColList[3]) *  as.numeric(ratio3) * pointValueInst3 * get(ColList[nCol-2])) + (get(ColList[4]) *  as.numeric(ratio4) * pointValueInst4 * get(ColList[nCol-1])) + (get(ColList[5])*  as.numeric(ratio5) * pointValueInst5 * get(ColList[nCol]))]
      FinalPortfolio[, c("SpreadRet", inst1, inst2, inst3, inst4, inst5) := list(c(0,diff((Spread))), c(0,diff(log(get(ColList[1]) * get(ColList[nCol-4])))), c(0,diff(log(get(ColList[2]) * get(ColList[nCol-3])))), c(0,diff(log(get(ColList[3]) * get(ColList[nCol-2])))), c(0,diff(log(get(ColList[4]) * get(ColList[nCol-1])))), c(0,logreturn1(get(ColList[5]) * get(ColList[nCol]))))]
    }
  } else {
    if(nInst==2){
      #print("2")
      
      FinalPortfolio[, Spread:= (get(ColList[1]) * as.numeric(ratio1)) + (get(ColList[2]) *  as.numeric(ratio2))]
      FinalPortfolio[, c("SpreadRet", inst1 , inst2) := list(c(0,diff((Spread))), c(0,diff(log(get(ColList[1])))), c(0,diff(log(get(ColList[2])))))]
    }
    if(nInst==3){
      #FinalPortfolio[, Spread:= (AD/.21) - (Close.1 * .12) - (Close.19 * 100)]
     
      FinalPortfolio[, Spread:= (FinalPortfolio$xtsData1.Close *  as.numeric(ratio1)) + (FinalPortfolio$xtsData2.Close *  as.numeric(ratio2)) + (FinalPortfolio$xtsData3.Close *  as.numeric(ratio3))]
      #FinalPortfolio[, Spread:= (get(ColList[1]) *  as.numeric(ratio1)) + (get(ColList[2]) *  as.numeric(ratio2)) / (get(ColList[3]) *  as.numeric(ratio3))]
      #FinalPortfolio[, Spread:= (get(ColList[1]) *  as.numeric(ratio1)) + (get(ColList[2]) *  as.numeric(ratio2)) + (get(ColList[3]) *  as.numeric(ratio3))]
      FinalPortfolio[, c("SpreadRet", inst1, inst2, inst3) := list(c(0,diff((Spread))), c(0,diff(log(get(ColList[1])))), c(0,diff(log(get(ColList[2])))), c(0,logreturn1(get(ColList[3]))))]
    }
    if(nInst==4){
      #print("4")
      FinalPortfolio[, Spread:= (get(ColList[1]) *  as.numeric(ratio1)) + (get(ColList[2]) *  as.numeric(ratio2)) + (get(ColList[3]) *  as.numeric(ratio3)) + (get(ColList[4]) *  as.numeric(ratio4))]
      FinalPortfolio[, c("SpreadRet", inst1, inst2, inst3, inst4) := list(c(0,diff((Spread))), c(0,diff(log(get(ColList[1])))), c(0,diff(log(get(ColList[2])))), c(0,diff(log(get(ColList[3])))), c(0,logreturn1(get(ColList[4]))))]
    }
    if(nInst==5){
      #print("5")
      FinalPortfolio[, Spread:= (get(ColList[1]) *  as.numeric(ratio1)) + (get(ColList[2]) *  as.numeric(ratio2)) + (get(ColList[3]) *  as.numeric(ratio3)) + (get(ColList[4]) *  as.numeric(ratio4)) + (get(ColList[5])*  as.numeric(ratio5))]
      FinalPortfolio[, c("SpreadRet", inst1, inst2, inst3, inst4, inst5) := list(c(0,diff((Spread))), c(0,diff(log(get(ColList[1])))), c(0,diff(log(get(ColList[2])))), c(0,diff(log(get(ColList[3])))), c(0,diff(log(get(ColList[4])))), c(0,logreturn1(get(ColList[5]))))]
    }
  }
  #************************* Make sure about the multiplier
  FinalPortfolio[, Spread:=Spread * multiplier]
  print(paste("Data Fetched..", "Took ", Sys.time()-system.time))  
  if(nrow(FinalPortfolio) > sma){
    FinalPortfolio[, Indicator:= SMA(Spread, sma)]
    na.omit(FinalPortfolio[, MADist:= (Spread-Indicator)])
  }
  print(tail(FinalPortfolio))
  incProgress(1/4)
#  print(paste("Indicator calculated..", "Took ", Sys.time()-system.time))
  #write.csv(FinalPortfolio, "C:/Users/Prashant/Dropbox/FinalPortfolioSeries.csv")
  write.csv(FinalPortfolio, "D:/SHINYPROJECT/StrategyAnalysis/FinalPortfolioSeries.csv")
  return(FinalPortfolio)##here
}

#head((DTPort$Close * 476.1) - (DTPort$Close.1 * 12) + (DTPort$Close.2 * 100))

CalculateNumberOfInstruments <- function(inst1,inst2,inst3,inst4,inst5){
  #inst1="TY"
  #inst2="XM"
 # inst3=""
  #inst4=""
  #inst5=""
  nInst<-0
  if(inst1 != "")
    nInst<-nInst+1
  if(inst2 != "")
    nInst<-nInst+1
  if(inst3 != "")
    nInst<-nInst+1
  if(inst4 != "")
    nInst<-nInst+1
  if(inst5 != "")
    nInst<-nInst+1
    return(nInst)
  } 

# '%then%' <- shiny:::'%OR%'

shinyServer(function(input, output,session) 
  {
    shinyServer(function(input, output, session){
      session$onSessionEnded(function() {
        stopApp()
        q("no") 
      })
    })
  # builds a reactive expression that only invalidates when the value of input$goButton becomes out of date (i.e., when the button is pressed)
    ntext <- eventReactive(input$goButton, {
      system.time1<- Sys.time()
      print(paste("Starting to prepare Raw data table..", system.time))
      #print(isolate(input$Inst1))
      #print(isolate(input$Inst2))
      withProgress(message = 'Fetching data and calculating final prices...', detail = "Please wait till finish...", value = .1, { 
        finalData<-createMasterTable(isolate(input$Date[1]),isolate(input$Date[2]),input$Inst1, input$Ratio1, input$Inst2, input$Ratio2, input$Inst3, input$Ratio3, input$Inst4, 
                        input$Ratio4, input$Inst5, input$Ratio5, input$Inst6, input$Inst7, input$Inst8, input$Inst9, input$Inst10, 
                        input$nInterval, input$Interval, isolate(input$rdoIndexOrBond), isolate(input$SMA), isolate(input$rdoDatabase),input$nMultiplier, input$nMaxBP, input$nMinBP)
      })
#       print("Finished creating Portfolio")
      print(paste("Raw data Table prepared..", "Took Total", Sys.time()-system.time1))
      return(finalData)
    })
  
    create4DAnalysisTable <- eventReactive(input$goButton, {
      #data<-fread("D:/SHINYPROJECT/StrategyAnalysis/FinalPortfolioSeries.csv")
      data<-ntext()
      withProgress(message = 'Getting Final Analysis table per Group...', detail = "Please wait till finish...", value = 0, { 
        finalData<-CeateMainGroupTable(data)
      })
      return(finalData)
    })

    getDataTableAbove <- eventReactive(input$goButton, {
      #data<-fread("D:/SHINYPROJECT/StrategyAnalysis/FinalPortfolioSeries.csv")
      mainTable <- create4DAnalysisTable()
      print("Enter getDataTableAbove")
      
      withProgress(message = 'Finding patterns above and below SMA...', detail = "Please wait till finish...", value = 0, { 
        dataTableAbove<-getFinalAnalysistablePerGroup(mainTable, "groupAbove", "durationAbove", 2, isolate(input$nFilter))  
         setnames(dataTableAbove, c("PeekRetace_PriceRatio", "PeekRetace_TimeRatio"),  c("PriceRatio_AboveSMA", "TimeRatio_AboveSMA"))
        print("Data table above")
        print(dataTableAbove)
      })
      return(dataTableAbove)
    })

    getDataTableBelow <- eventReactive(input$goButton, {
      #data<-fread("D:/SHINYPROJECT/StrategyAnalysis/FinalPortfolioSeries.csv")
      mainTable <- create4DAnalysisTable()
      print("Enter getDataTableBelow")
      
      withProgress(message = 'Finding patterns above and below SMA...', detail = "Please wait till finish...", value = 0, { 
        dataTableBelow<-getFinalAnalysistablePerGroup(mainTable, "groupBelow", "durationBelow", 2, isolate(input$nFilter))  
         setnames(dataTableBelow, c("PeekRetace_PriceRatio", "PeekRetace_TimeRatio"),  c("PriceRatio_BelowSMA", "TimeRatio_BelowSMA"))
        print("Data table below")
        print(dataTableBelow)
      })
      return(dataTableBelow)
    })

    CalulateProfitInEach_BelowPattern_Reactive <- eventReactive(input$goButton, {
      #data<-fread("D:/SHINYPROJECT/StrategyAnalysis/FinalPortfolioSeries.csv")
      mainTable <- create4DAnalysisTable()
      print("Enter CalulateProfitInEach_BelowPattern_Reactive")
      withProgress(message = 'Calculating BP for each pattern below SMA...', detail = "Please wait till finish...", value = 0, { 
        dataTableBelow<-CalulateProfitInEach_BelowPattern_Loop(groupTable, 2, input$nMinBP, input$nMaxBP)  
      })
      return(dataTableBelow)
    })
    
    CalulateProfitInEach_AbovePattern_Reactive <- eventReactive(input$goButton, {
      #data<-fread("D:/SHINYPROJECT/StrategyAnalysis/FinalPortfolioSeries.csv")
      mainTable <- create4DAnalysisTable()
      print("Enter CalulateProfitInEach_AbovePattern_Reactive")
      withProgress(message = 'Calculating BP for each pattern above SMA...', detail = "Please wait till finish...", value = 0, { 
        dataTableBelow<-CalulateProfitInEach_AbovePattern_Loop(groupTable, 2, input$nMinBP, input$nMaxBP)  
      })
      return(dataTableBelow)
    })
    
    getValueData <- reactive( {
      #data<-fread("D:/SHINYPROJECT/StrategyAnalysis/FinalPortfolioSeries.csv")
      data<-ntext()
      thresholdVal <-input$nFilter
      filteredValueData<-data[abs(MADist) > thresholdVal, ]
      return(filteredValueData)
    })
    
    CalulatePartitionGroup_Reactive <- eventReactive(input$partitionButton,{
      print("Enter CalulatePartitionGroup_Reactive")
      data<-ntext()
      lookbackPeriod=(isolate(input$LookBackPeriod))+1
      byEvery=(isolate(input$ByEvery))
      lookbackDaysPast =isolate(-input$DaysBefore)
      lookbackDaysFuture =isolate(input$DaysAfter)
      referenceDate <-ymd(isolate(input$ReferenceDate))
      groupedData<-as.data.table(CreatePartitiongGroupTable(data, lookbackPeriod, byEvery, lookbackDaysPast,lookbackDaysFuture,referenceDate))
      write.csv(groupedData, "D:/SHINYPROJECT/StrategyAnalysis/PartitionGroupdata.csv")
      return(groupedData)
    })
    # 
    ###########################################################   Price Plot
    
    output$PricePlot <-  renderDygraph({
      ################################ observeEvent works only under the function and will be called when the event happen although
      # eventReactive can call funtions 
#       observeEvent(input$goButton, {
#         #print("enabling tab")
#         updateTabsetPanel(session, "tabPanels", selected = "panel1")
#       })
      #ntext()
#       data<-read.csv("D:/SHINYPROJECT/StrategyAnalysis/FinalPortfolioSeries.csv")
      system.time<- Sys.time()
      print(paste("Render DyGraph..",system.time))

      data<-as.data.frame(ntext())
      withProgress(message = 'Ploting Chart..', detail = "Please wait till finish..", value = 0 , max=3, { 
        #data<-read.csv("FinalPortfolioSeries.csv")
        incProgress(1)
         SMALength<-input$SMA
        Interval<-isolate(input$Interval)
        #print(length(data[,"Spread"]))
        if(length(data[,"Spread"]) < SMALength)
        {
          #print("1")
          if(Interval=="daily" || Interval=="weekly")
            xxts<-as.xts(data[,"Spread"], ymd(strftime(data[,"TradeTime"],"%F"), tz="Asia/Kuala_Lumpur"))
          else
            xxts<-as.xts(data[,"Spread"], ymd_hms(data[,"TradeTime"], tz="Asia/Kuala_Lumpur"))
          
          names(xxts)<-c("Spread")
        } else {
          #print("2")
          MA200<-runMean(data[,"Spread"], n = SMALength, cumulative = FALSE)
          if(Interval=="daily" || Interval=="weekly")
            xxts<-as.xts(cbind(data[,"Spread"], MA200),  ymd(strftime(data[,"TradeTime"],"%F"), tz="Asia/Kuala_Lumpur"))
          else
            xxts<-as.xts(cbind(data[,"Spread"], MA200), ymd_hms(data[,"TradeTime"], tz="Asia/Kuala_Lumpur"))
          
          names(xxts)<-c("Spread","SMA")
        }
        incProgress(2)
        print(paste("DyGraph rendered..", "Took ", Sys.time()-system.time))
        if(length(data[,"Spread"]) > SMALength){
        dygraph(xxts) %>%
          dySeries(c("Spread"), label = c("Price"), drawPoints = F) %>%
          dySeries(c("SMA"), label = "SMA")%>%
          dyOptions(drawGrid = TRUE) %>%
          dyRangeSelector()
        } else {
          dygraph(xxts) %>%
          dySeries(c("Spread"), label = c("Price"))%>%
          dyOptions(drawGrid = TRUE) %>%
          dyRangeSelector()
        }
        
      })
      
      #print("Exit PricePlot")
         # dyOptions(stackedGraph = TRUE) %>%
        #dyRangeSelector()
      
      #chartSeries(xxts, theme = chartTheme("white"), type = "line", TA = NULL) 
     #addSMA(n=input$SMA, col="red")
    })
    
    output$priceTable <- renderDataTable({
      print("Enter Render Table")
      #data<-head(as.data.frame(read.csv("D:/SHINYPROJECT/StrategyAnalysis/FinalPortfolioSeries.csv")))
      data<-as.data.frame(ntext())
      if (is.null(data())) {return()}
      #print(tail(df,1))
      print("Exit Render Table")
      colList<-grep("xts", names(data), value=T)
      totalInstColumns <- length(colList)
      cols<-merge(rep("Inst", 1), seq(1, totalInstColumns))
      newInst_ColNames=do.call(paste, c(as.list(cols), sep=""))
      Data<-rbind(head(data[,c("TradeTime", colList, "Spread")],1), tail(data[,c("TradeTime", colList, "Spread")],1))
      setnames(Data, c("TradeTime", colList, "Spread"), c("TradeTime", newInst_ColNames, "Spread"))
      Data
      #print("out")
    }, 
    options = list(lengthChange = FALSE)
    )

    ###########################################################   Value Prob Plot

    output$Value_Price_ProbChart <- renderPlot({
      referencePoint<-as.numeric(input$RefValue)
      filteredData<-getValueData()
      Calculate_Vaue_Probability (filteredData,referencePoint, "Value prob plot (Virtical distance)", "Basis Points", isolate(input$SMA))
      #print("Exit ProbPlot")
      #data<-read.csv("D:/SHINYPROJECT/StrategyAnalysis/FinalPortfolioSeries.csv")
      #calculateProbability (as.data.table(data))
     })

    output$ExtreamValueTable_Value <- renderDataTable({
      print("Enter Prob Render Table")
      #       data<-fread("D:/SHINYPROJECT/StrategyAnalysis/FinalPortfolioSeries.csv")
      #data<-as.data.table(ntext())
      #       if (is.null(data())) {return()}
      #        extreamValue<-input$ExtreamValue
      #       extreamValueDT<-data %>%
      #         filter(abs(MADist)>abs(extreamValue)) %>%
      #         select(c(TradeTime, Spread, MADist)) %>%
      #         arrange(desc(MADist))
      data<-as.data.table(ntext())
      data[, TradeDate:=as.Date(TradeTime)]
      if (is.null(data())) {return()}
      extreamValue<-as.numeric(input$ExtreamValue)
      extreamValueDT<-data %>%
        filter(abs(MADist)>abs(extreamValue)) %>%
        group_by(TradeDate) %>%
        slice(which.max(abs(MADist))) %>%
        select(c(TradeDate, MADist)) %>%
        arrange(desc(MADist))
      print("Exit Prob Render Table")
      print(extreamValueDT)
      extreamValueDT
    }, 
    options = list(lengthChange = FALSE)
    )
    
    output$rollingRegression <- renderPlot({
      # Sample data
      #library(quantmod)
      #getSymbols("^GSPC", from="2009-01-01")
      data<-fread("D:/SHINYPROJECT/StrategyAnalysis/FinalPortfolioSeries.csv")
      #data<-as.data.frame(data)#ntext())
      xtsData <- as.xts(data[,Spread],  ymd_hms(data[,TradeTime], tz="GMT"))
      # Rolling regression (unweighted), with prediction intervals
      x <- rollapplyr( 
        as.zoo(data[,Spread]), 
        width=200, by.column = FALSE, 
        FUN = function(x) {
          r <- lm( x ~ index(x) )
          tail(predict(r, interval="prediction"),1)
        } 
      )
      # Plots
      plot( index(xtsData), data[,Spread], type="l", lwd=3, las=1 )
      lines( index(x), x$fit, col="purple", lwd=3 )
      lines( index(x), x$lwr, col="purple", lwd=3, lty=3 )
      lines( index(x), x$upr, col="purple", lwd=3, lty=3 )
      abline( lm( data[,Spread] ~ index(xtsData) ), col="light blue", lwd=3)  
      
      fit<-c(rep(NA,199), x$fit)
      factorTable<-data[ , .(TradeTime, Spread, SpreadRet, MADist)][, LRFit:=fit1][, c("LR_MA","Prc_LR",  "UD") :=.(LRFit-MADist, Spread-LRFit,  as.factor(ifelse(SpreadRet>0, "1","0")))]
      factorTable1<-factorTable[200:nrow(factorTable),.SD][, .(MADist, LR_MA,Prc_LR,  UD)]
      
      write.csv(fit1, "D:/SHINYPROJECT/StrategyAnalysis/RollingRegression.csv")
      write.csv(factorTable1, "D:/SHINYPROJECT/StrategyAnalysis/FactorTable.csv")
      table(factorTable1$UD)/nrow(factorTable1)
      sample.ind <- sample(2, 
                           nrow(factorTable1),
                           replace = T,
                           prob = c(0.6,0.4))
      cross.sell.dev <- factorTable1[sample.ind==1,]
      cross.sell.val <- factorTable1[sample.ind==2,]
      
      table(cross.sell.dev$UD)/nrow(cross.sell.dev)
      
      table(cross.sell.val$UD)/nrow(cross.sell.val)
      class(cross.sell.dev$UD)
      
      varNames <- names(cross.sell.dev)
      # Exclude ID or Response variable
      varNames <- varNames[!varNames %in% c("TradeTime","Spread","SpreadRet", "UD")]
      
      # add + sign between exploratory variables
      varNames1 <- paste(varNames, collapse = "+")
      
      # Add response variable and convert to a formula object
      rf.form <- as.formula(paste("UD", varNames1, sep = " ~ "))
      library(randomForest)
      cross.sell.rf <- randomForest(rf.form,
                                    cross.sell.dev,
                                    ntree=200,
                                    importance=T)
      cross.sell.rf
      plot(cross.sell.rf)
      
      # Variable Importance Plot
      varImpPlot(cross.sell.rf,
                 sort = T,
                 main="Variable Importance",
                 n.var=3)
      
      # Variable Importance Table
      var.imp <- data.frame(importance(cross.sell.rf,
                                       type=1))
      # make row names as columns
      var.imp$Variables <- row.names(var.imp)
      var.imp[order(var.imp$MeanDecreaseAccuracy,decreasing = T),]
      
      # Predicting response variable
      cross.sell.dev$predicted.response <- predict(cross.sell.rf ,cross.sell.dev)
      library(e1071)
      library(caret)
      ## Loading required package: lattice
      ## Loading required package: ggplot2
      # Create Confusion Matrix
      a<-cross.sell.dev$predicted.response
      b<-cross.sell.dev$UD
      confusionMatrix(data=cross.sell.dev$predicted.response,
                      reference=cross.sell.dev$UD)
    })
    
    ############################################################  Risk Prob Plot
    
    output$Risk_Price_ProbChart <- renderPlot({
      referencePoint<-as.numeric(input$RefValue)
      #print(referencePoint)
      #data<-ntext()
      #data<-fread("D:/SHINYPROJECT/StrategyAnalysis/FinalPortfolioSeries.csv")
      #mainTable=finalData
      #referencePoint=1
      print("Price Probability")
      dataTableAbove<-getDataTableAbove()#getFinalAnalysistablePerGroup(mainTable, "groupAbove", "durationAbove", 2, isolate(input$nFilter))   
      dataTableBelow<-getDataTableBelow()#getFinalAnalysistablePerGroup(mainTable, "groupBelow", "durationBelow", 2, isolate(input$nFilter))   
      showAboveFilter<-as.numeric(input$nFilter) 
      calculateProbabilityofCycles(dataTableAbove, dataTableBelow, "T2_T1_PriceCng", referencePoint,showAboveFilter,'Price Change from T1- T2', 'Price Change per Cycle', 'Price Change Above MA', 'Price Change Below MA')
    })
    
    output$Risk_Time_ProbChart <- renderPlot({
      referencePoint<-as.numeric(input$RefValue)
      #print(referencePoint)
      #data<-ntext()
      #data<-fread("D:/SHINYPROJECT/StrategyAnalysis/FinalPortfolioSeries.csv")
      #mainTable=finalData
      #referencePoint=1
      dataTableAbove<-getDataTableAbove()#getFinalAnalysistablePerGroup(mainTable, "groupAbove", "durationAbove", 2, isolate(input$nFilter))  
      dataTableBelow<-getDataTableBelow()#getFinalAnalysistablePerGroup(mainTable, "groupBelow", "durationBelow", 2, isolate(input$nFilter))   
      showAboveFilter<-as.numeric(input$nFilter) 
      calculateProbabilityofCycles(dataTableAbove, dataTableBelow, "T1_T3_Bars", referencePoint,showAboveFilter,"Number of Bars from T1- T3","Number of Bars per Cycle", "Cycle Above MA","Cycle Below MA")
    })
    
    output$ExtreamValueTable_Risk <- renderDataTable({
      print("Enter Prob Render Table")
      #       data<-fread("D:/SHINYPROJECT/StrategyAnalysis/FinalPortfolioSeries.csv")
      dataTableAbove<-getDataTableAbove()#getFinalAnalysistablePerGroup(mainTable, "groupAbove", "durationAbove", 2, isolate(input$nFilter))    
      dataTableBelow<-getDataTableBelow()#getFinalAnalysistablePerGroup(mainTable, "groupBelow", "durationBelow", 2, isolate(input$nFilter))   
      
      l<-list(dataTableAbove, dataTableBelow)
      mergedAboveBelow<-rbindlist(l, use.names=T, fill=T)
      TabletoPrint<-mergedAboveBelow[,.(StartTime, T2_T1_PriceCng, T3_T2_PriceCng, PriceRatio_AboveSMA, PriceRatio_BelowSMA,T1_T2_Bars, T2_T3_Bars, TimeRatio_AboveSMA, TimeRatio_BelowSMA)]
      
      if (is.null(mergedAboveBelow)) {return()}
      TabletoPrint
    })
    
    ##############################################################  Summary Plot
    
    output$SummaryAbove_Risk<- renderTable({
      print("Enter SummaryAbove_Risk")
      #data<-ntext()
#       data<-fread("D:/SHINYPROJECT/StrategyAnalysis/FinalPortfolioSeries.csv")
      dataTableAbove<-getDataTableAbove()#getFinalAnalysistablePerGroup(mainTable, "groupAbove", "durationAbove", 2, isolate(input$nFilter))  
      showAboveFilter<-as.numeric(input$nFilter) 
      stats<-calculateSummaryAbove (dataTableAbove[T2_T1_PriceCng > showAboveFilter,T2_T1_PriceCng])
      fStats<-lapply(stats, format_num)
      dtStats<-as.data.table(t(sapply(fStats, cbind)), keep.rownames=F)
#       setnames(dtStats,names(stats))
#       as.data.frame(ids=names(stats), nums=t)
      print(dtStats)
      return(dtStats)
    })

    output$SummaryBelow_Risk<- renderTable({
      print("Enter SummaryBelow_Risk")
     # data<-ntext()
     mainTable <- create4DAnalysisTable()
     dataTableBelow<-getDataTableBelow()#getFinalAnalysistablePerGroup(mainTable, "groupBelow", "durationBelow", 2, isolate(input$nFilter))   
     showAboveFilter<-as.numeric(input$nFilter) 
      stats<-calculateSummaryBelow (dataTableBelow[T2_T1_PriceCng > showAboveFilter, T2_T1_PriceCng])
      #fStats<-lapply(stats, format_num)
      dtStats<-as.data.table(t(sapply(stats, cbind)), keep.rownames=F)
      print(dtStats)
      #print(t(sapply(ss, cbind)))
      return(dtStats)
    }, digits=4)
    
    output$SummaryAbove_Value<- renderTable({
      print("Enter SummaryAbove_Value")
      data<-getValueData()
      
      PositiveDT<-data[MADist > 0,MADist]
      stats<-calculateSummaryAbove (PositiveDT)
      fStats<-lapply(stats, format_num)
      dtStats<-as.data.table(t(sapply(fStats, cbind)), keep.rownames=F)
      #       setnames(dtStats,names(stats))
      #       as.data.frame(ids=names(stats), nums=t)
      print(dtStats)
      return(dtStats)
    })
    
    output$SummaryBelow_Value<- renderTable({
      print("Enter SummaryBelow_Value")
      data<-getValueData()
      
      NegetiveDT<-data[MADist < 0,-MADist]
      stats<-calculateSummaryBelow (NegetiveDT)
      #fStats<-lapply(stats, format_num)
      dtStats<-as.data.table(t(sapply(stats, cbind)), keep.rownames=F)
      print(dtStats)
      #print(t(sapply(ss, cbind)))
      return(dtStats)
    }, digits=4)

    format_num <- function(col) {
      if (is.numeric(col))
        sprintf('%1.4f', col)
      else
        col
    }
    
    output$BoxPlot_Risk<- renderPlot({
      #data<-fread("D:/SHINYPROJECT/StrategyAnalysis/FinalPortfolioSeries.csv")
      #print("Enter BoxPlot")
      #data<-ntext()  //old
      #summ<-calculateSummaryPlot (data,isolate(input$SMA))[,-4]//old
      
      print("Entered BoxPlot")
      dataTableAbove<-getDataTableAbove()#getFinalAnalysistablePerGroup(mainTable, "groupAbove", "durationAbove", 2)  
      dataTableBelow<-getDataTableBelow()#getFinalAnalysistablePerGroup(mainTable, "groupBelow", "durationBelow", 2)  
      showAboveFilter<-as.numeric(input$nFilter) 
      summ<-calculateSummaryPlot(dataTableAbove[T2_T1_PriceCng > showAboveFilter,T2_T1_PriceCng], dataTableBelow[T2_T1_PriceCng > showAboveFilter,T2_T1_PriceCng])
      
      m <- melt(summ)
      m<-arrange(m, desc(Var1), value)
      print(m)
      #m <- transform(m,variable=reorder(Var1,value))
      #m <- transform(m,variable=reorder(variable,value))
      #m$variable <- with(m, relevel(var1,value))
      #ggplot(m,aes(x=factor(Var1),y=value))+geom_boxplot(notch=F,outlier.colour="red", outlier.shape=8,
      #                                                   outlier.size=4)+coord_flip()+#xlim("AboveSMA", "BelowSMA")+
      p <- ggplot(data = m, aes(x=factor(Var1), y=value)) 
      p <- p + geom_boxplot(aes(fill=factor(Var1)))
      p <- p + scale_x_discrete(limits=c("BelowSMA", "AboveSMA"))+coord_flip()
      p <- p + guides(fill=guide_legend(title="Legend_Title"))
      
      if(input$rdoIndexOrBond=="Index Model")
        p <- p + labs(title="",y="Dollar($)", x = "Above or Below MA")
      else
        p <- p + labs(title="",y="Basis Points", x = "Above or Below MA")
      p
    })
    
    output$BoxPlot_Value<- renderPlot({
      #data<-fread("D:/SHINYPROJECT/StrategyAnalysis/FinalPortfolioSeries.csv")
      #print("Enter BoxPlot")
      #data<-ntext()  //old
      #summ<-calculateSummaryPlot (data,isolate(input$SMA))[,-4]//old
      
      print("Entered BoxPlot_Value")
      data<-getValueData()
      
      PositiveDT<-data[MADist>0,MADist]
      NegetiveDT<-data[MADist<0,-MADist]
      
      summ<-calculateSummaryPlot(PositiveDT, NegetiveDT)
      
      m <- melt(summ)
      m<-arrange(m, desc(Var1), value)
      print(m)
      #m <- transform(m,variable=reorder(Var1,value))
      #m <- transform(m,variable=reorder(variable,value))
      #m$variable <- with(m, relevel(var1,value))
      #ggplot(m,aes(x=factor(Var1),y=value))+geom_boxplot(notch=F,outlier.colour="red", outlier.shape=8,
      #                                                   outlier.size=4)+coord_flip()+#xlim("AboveSMA", "BelowSMA")+
      p <- ggplot(data = m, aes(x=factor(Var1), y=value)) 
      p <- p + geom_boxplot(aes(fill=factor(Var1)))
      p <- p + scale_x_discrete(limits=c("BelowSMA", "AboveSMA"))+coord_flip()
      p <- p + guides(fill=guide_legend(title="Legend_Title"))
      
      if(input$rdoIndexOrBond=="Index Model")
        p <- p + labs(title="",y="Dollar($)", x = "Above or Below MA")
      else
        p <- p + labs(title="",y="Basis Points", x = "Above or Below MA")
      p
    })
    
    output$BoxPlotOldProb<- renderPlot({
      #data<-fread("D:/SHINYPROJECT/StrategyAnalysis/FinalPortfolioSeries.csv")
      #print("Enter BoxPlot")
      data<-ntext()  
      summ<-calculateSummaryPlot (data,isolate(input$SMA))[,-4]
      #summ<-calculateSummaryPlot (data,200)[,-4]
      
      m <- melt(summ)
      m<-arrange(m, desc(Var1,value))
      #m <- transform(m,variable=reorder(Var1,value))
      #m <- transform(m,variable=reorder(variable,value))
      #m$variable <- with(m, relevel(var1,value))
      ggplot(m,aes(x=factor(Var1),y=value))+geom_boxplot(notch=F,outlier.colour="red", outlier.shape=8,
                                                         outlier.size=4)+coord_flip()+xlim("AboveSMA", "BelowSMA")+
        if(input$rdoIndexOrBond=="Index Model")
          labs(title="",y="Dollar($)", x = "Above or Below MA")
      else
        labs(title="",y="Basis Points", x = "Above or Below MA")
    })
    
    ################################################################## PARTITIONING

    output$BeforeSegmentPlot<- renderPlot({
      #data<-fread("D:/SHINYPROJECT/StrategyAnalysis/FinalPortfolioSeries.csv")
      print("Enter BeforeSegmentPlot")
      #data<-ntext()
        #lookbackPeriod=(input$LookBackPeriod)
        #byEvery=(input$ByEvery)
        lookbackDaysPast =(isolate(-input$DaysBefore))
        lookbackDaysFuture =(isolate(input$DaysAfter))
        referenceDate <-ymd(isolate(input$ReferenceDate))
        #groupedData<-CalulatePartitionGroup_Reactive()
        groupedData <-fread("D:/SHINYPROJECT/StrategyAnalysis/PartitionGroupdata.csv")
        
        #groupedData<-as.data.table(CreatePartitiongGroupTable(data, lookbackPeriod, byEvery, lookbackDaysPast,lookbackDaysFuture,referenceDate))
      #groupedData1<-as.data.table(CreatePartitiongGroupTable(data, 5, "-3 month", -15,15,ymd('2017-09-16')))
      #groupedData2<-fread("D:/SHINYPROJECT/StrategyAnalysis/PartitionDataWithGroups.csv")
      data<-NULL
      df_melt = melt(groupedData[subGroup == "Before", ],  measure.vars='Change')
      write.csv(df_melt, "D:/SHINYPROJECT/StrategyAnalysis/BeforeData.csv")
      print("melted---")
      #head(df_melt)
      ggplot(df_melt, aes(x= as.numeric(V1), y = value)) + 
        geom_line(color="green4") + 
        xlab(paste("Each segment showing ", lookbackDaysPast," days before the Reference Date " , referenceDate, " each period")) + ylab("Change in BP")+ ggtitle(paste(lookbackDaysPast,"Before the Reference Date"))+
        facet_grid(~ group, scales = 'free_x', as.table=F)
    })
    
    output$AfterSegmentPlot<- renderPlot({
      #data<-fread("D:/SHINYPROJECT/StrategyAnalysis/FinalPortfolioSeries.csv")
      print("Enter AfterSegmentPlot")
      #data<-ntext()  #old
      # lookbackYear=4+1
      # byEvery="-1 year"
       # lookbackDaysPast =-30
       # lookbackDaysFuture =30
       # referenceDate <-ymd('2016-04-16')

       lookbackPeriod=(isolate(input$LookBackPeriod))
       byEvery=(isolate(input$ByEvery))
       lookbackDaysPast =(-isolate(input$DaysBefore))
       lookbackDaysFuture =(isolate(input$DaysAfter))
       referenceDate <-ymd(isolate(input$ReferenceDate))
       #groupedData<-as.data.table(CreatePartitiongGroupTable(data, lookbackPeriod, byEvery, lookbackDaysPast,lookbackDaysFuture,referenceDate))
       data<-NULL
       #groupedData<-CalulatePartitionGroup_Reactive()
       groupedData <-fread("D:/SHINYPROJECT/StrategyAnalysis/PartitionGroupdata.csv")
      #print(groupedData)
      #groupedData2<-fread("D:/SHINYPROJECT/StrategyAnalysis/PartitionDataWithGroups.csv")
      
      df_melt = melt(groupedData[subGroup == "After", ],  measure.vars='Change')
      write.csv(df_melt, "D:/SHINYPROJECT/StrategyAnalysis/AfterData.csv")
      print("melted---")
      #head(df_melt)
      ggplot(df_melt, aes(x= as.numeric(V1), y = value)) + 
        geom_line( color="blue4") + 
        xlab(paste("Each segment showing ", lookbackDaysFuture," days after the Reference Date " , referenceDate, " each period")) + ylab("Change in BP")+ ggtitle(paste(lookbackDaysFuture,"After the Reference Date"))+
        facet_grid(~ group, scales = 'free_x', as.table=F)
      
    })
    
    output$SegmentResults<- renderTable({
      print("Enter PartitionResults")
      #groupedData<-CalulatePartitionGroup_Reactive()
      groupedData <-fread("D:/SHINYPROJECT/StrategyAnalysis/PartitionGroupdata.csv")
      #eachGroupRange <- groupedData[ , .(Range = max(Spread)- min(Spread), std=round(sd(Change),3)), by= .(group, subGroup)]
      #eachGroupRange <- groupedData[ , .(Slope = max(Spread)- min(Spread), std=round(sd(Change),3)), by= .(group, subGroup)]
      eachGroupRange <- groupedData[ , .(Slope =  coef(lm(Spread ~ seq(1,length(Spread))))[2], Range = max(Spread)- min(Spread), std=round(sd(Change),3)), by= .(group, subGroup)]    
      
      #melt(eachGroupRange[subGroup=='Before', ], id.vars = c("group", "subGroup"))
      beforeStatsByGroup <- eachGroupRange[subGroup=='Before', .(group, round(Slope,2), round(Range,1), round(std,1))]
      afterStatsByGroup <- eachGroupRange[subGroup=='After', .(group, round(Slope,2), round(Range,1), round(std,1))]
      mergeResults <- merge(beforeStatsByGroup, afterStatsByGroup, by.x="group", by.y="group", sort=F)
      setnames(mergeResults, c("group", "V2.x", "V3.x", "V4.x","V2.y", "V3.y", "V4.y"), c("Month", "Before.Slope","Before.Range", "Before.STD", "After.Slope", "After.Range", "After.STD"))
      print(mergeResults)
      
      #stats<-apply(beforeStatsByGroup[, .(Range, std)], 2, mean)
      
      # PositiveDT<-data[MADist > 0,MADist]
      # stats<-calculateSummaryAbove (PositiveDT)
      # fStats<-lapply(stats, format_num)
      # dtStats<-as.data.table(t(sapply(fStats, cbind)), keep.rownames=F)
      # setnames(dtStats,names(stats))
      # as.data.frame(ids=names(stats), nums=t)
      # print(dtStats)
      return(mergeResults)
    })
    
    output$SegmentAverageResults<- renderTable({
      print("Enter SegmentAverageResults")
      #groupedData<-CalulatePartitionGroup_Reactive()
      groupedData <-fread("D:/SHINYPROJECT/StrategyAnalysis/PartitionGroupdata.csv")
      #print(groupedData)
      #eachGroupRange <- groupedData[ , .(Range = max(Spread)- min(Spread), std=round(sd(Change),3)), by= .(group, subGroup)]
      eachGroupRange <- groupedData[ , .(Slope =  coef(lm(Spread ~ seq(1,length(Spread))))[2], Range = max(Spread)- min(Spread), std=round(sd(Change),3)), by= .(group, subGroup)]    
      
      afterStatsByGroup <- eachGroupRange[subGroup=='After', .(group, Slope, Range, std)]
      print(afterStatsByGroup)
      beforeStatsByGroup <- eachGroupRange[subGroup=='Before', .(group, Slope, Range, std)]
      print(beforeStatsByGroup)
      
      beforeStats<- round(apply(beforeStatsByGroup[, .(Range, std)], 2, mean),1)
      afterStats<-round(apply(afterStatsByGroup[, .(Range, std)], 2, mean),1)
      #dtStats<- round(t(cbind(beforeStats, afterStats)),3)
      
      beforeSlope <- paste0(round(table(beforeStatsByGroup[, Slope > 0]) /length(beforeStatsByGroup[, Slope]),2) *100 ,"%")
      afterSlope <- paste0(round(table(afterStatsByGroup[, Slope > 0]) /length(afterStatsByGroup[, Slope]),2) *100 ,"%")
      names(beforeSlope) <- c("prob(Down)", "prob(UP)")
      names(afterSlope) <- c("prob(Down)", "prob(UP)")
      print(beforeSlope)
      print(afterSlope)
      x<-as.matrix(as.list(beforeSlope))
      y<-as.matrix(as.list(beforeStats))
      combineBeforestats <- rbind(x,y)
      
      x<-as.matrix(as.list(afterSlope))
      y<-as.matrix(as.list(afterStats))
      combineAfterstats <- rbind(x,y)
      
      BeforeAfterCombineTable <- cbind(combineBeforestats, combineAfterstats)
      colnames(BeforeAfterCombineTable)<-c("Before Rolls", "After Rolls")
      print(BeforeAfterCombineTable)
      return(BeforeAfterCombineTable)
    },rownames = TRUE)
    
    output$SegmentCrossComapare<- renderTable({
      print("Enter SegmentResultsAfter")
      #groupedData<-CalulatePartitionGroup_Reactive()
      groupedData <-fread("D:/SHINYPROJECT/StrategyAnalysis/PartitionGroupdata.csv")
      eachGroupRange <- groupedData[ , .(Slope =  coef(lm(Spread ~ seq(1,length(Spread))))[2], Range = max(Spread)- min(Spread), std=round(sd(Change),3)), by= .(group, subGroup)]    
      
      beforeStatsByGroup <- eachGroupRange[subGroup=='Before', .(group, round(Slope,2), round(Range,1), round(std,1))]
      afterStatsByGroup <- eachGroupRange[subGroup=='After', .(group, round(Slope,2), round(Range,1), round(std,1))]
      mergeResults <- merge(beforeStatsByGroup, afterStatsByGroup, by.x="group", by.y="group")
      setnames(mergeResults, c("group", "V2.x", "V3.x", "V4.x","V2.y", "V3.y", "V4.y"), c("Month", "Before.Slope","Before.Range", "Before.STD", "After.Slope", "After.Range", "After.STD"))
      print(mergeResults)
      BeforeUP_AfterUP <- round(count(mergeResults[Before.Slope > 0 & After.Slope > 0,]) /count(mergeResults[Before.Slope > 0,]) * 100, 2)
      BeforeUP_AfterDown <- round(count(mergeResults[Before.Slope > 0 & After.Slope < 0,]) /count(mergeResults[Before.Slope > 0,]) * 100, 2)
      BeforeDown_AfterUP <- round(count(mergeResults[Before.Slope < 0 & After.Slope > 0,]) /count(mergeResults[Before.Slope < 0,]) * 100, 2)
      BeforeDown_AfterDown <- round(count(mergeResults[Before.Slope < 0 & After.Slope < 0,]) /count(mergeResults[Before.Slope < 0,]) * 100, 2)
      cData<-cbind(BeforeUP_AfterUP,BeforeUP_AfterDown,BeforeDown_AfterUP,BeforeDown_AfterDown)
      finalUPDownMatrix <- matrix(cData,2,2, byrow=F,
                                  dimnames = list(c("BeforeUP", "BeforeDown"),
                                                  c("AfterUP", "AfterDown")
                                  )  
      )
      print(finalUPDownMatrix)
      
      # PositiveDT<-data[MADist > 0,MADist]
      # stats<-calculateSummaryAbove (PositiveDT)
      # fStats<-lapply(stats, format_num)
      # dtStats<-as.data.table(t(sapply(fStats, cbind)), keep.rownames=F)
      # setnames(dtStats,names(stats))
      # as.data.frame(ids=names(stats), nums=t)
      # print(dtStats)
      return(finalUPDownMatrix)
    }, rownames = TRUE)
    ###################################################################  Bin Tables 
    
    output$BinsTable_Value = renderDataTable({
      print("Print BinsTable_Value")
      data<-getValueData()
      
      PositiveDT<-data[MADist>0,MADist]
      NegetiveDT<-data[MADist<0,-MADist]
      
      binsTable<-calculateBins(PositiveDT, NegetiveDT, input$Normalize, input$BinsBy)
      binsTable
    } , options = list(
      lengthChange = FALSE,
      paging = FALSE,
      searching=FALSE,
      #         initComplete = JS(
      #           "function(settings, json) {",
      #           "$(this.api().table().header()).css({'background-color': '#42f', 'color': '#fff'});",
      #           "}"),
      autowidth = FALSE
      #columnDefs = list(list(width = '20%', targets = 1),list(width = '20%', targets = 2),list(width = '20%', targets = 3))
    )
    )
    
    output$BinsTable_Risk = renderDataTable({
      print("Print Bins")
      dataTableAbove<-getDataTableAbove()#getFinalAnalysistablePerGroup(mainTable, "groupAbove", "durationAbove", 2, isolate(input$nFilter))   
      dataTableBelow<-getDataTableBelow()#getFinalAnalysistablePerGroup(mainTable, "groupBelow", "durationBelow", 2, isolate(input$nFilter))  
      showAboveFilter<-as.numeric(input$nFilter) 
      binsTable<-calculateBins(dataTableAbove[T2_T1_PriceCng > showAboveFilter, T2_T1_PriceCng], dataTableBelow[T2_T1_PriceCng > showAboveFilter,T2_T1_PriceCng], input$Normalize, input$BinsBy)
      binsTable
    } , options = list(
      lengthChange = FALSE,
      paging = FALSE,
      searching=FALSE,
      #         initComplete = JS(
      #           "function(settings, json) {",
      #           "$(this.api().table().header()).css({'background-color': '#42f', 'color': '#fff'});",
      #           "}"),
      autowidth = FALSE
      #columnDefs = list(list(width = '20%', targets = 1),list(width = '20%', targets = 2),list(width = '20%', targets = 3))
    )
    )
    #################################################################################   Extream ARROWs
    
    output$Arrow_StartToEnd <- renderPlot({
      #data<-fread("D:/SHINYPROJECT/StrategyAnalysis/FinalPortfolioSeries.csv")
      #mainTable=finalData
      print("calculating Arrow_StartToEnd")
      mainTable <- create4DAnalysisTable()
      print("calculating main done")
      dataTableAbove<-getSignedFinalAnalysistablePerGroup(mainTable, "groupAbove", "durationAbove", 2, input$nFilter) 
      dataTableBelow<-getSignedFinalAnalysistablePerGroup(mainTable, "groupBelow", "durationBelow", 2, input$nFilter)
      l<-list(dataTableAbove, dataTableBelow)
      mergedAboveBelow<-rbindlist(l, use.names=T, fill=T)
      #write.csv(mergedAboveBelow, "D:/SHINYPROJECT/StrategyAnalysis/MergedArrorData.csv")
      g2<-CreateArrorChartsForSpeed(mergedAboveBelow)
      g2
    })
    
    output$Arrow_Extream <- renderPlot({
      #data<-fread("D:/SHINYPROJECT/StrategyAnalysis/FinalPortfolioSeries.csv")
      #mainTable=finalData

      print("calculating Arrow_Extream")
      mainTable <- create4DAnalysisTable()
      print("calculating main done")
      dataTableAbove<-getSignedFinalAnalysistablePerGroup(mainTable, "groupAbove", "durationAbove", 3, input$nFilter) 
      dataTableBelow<-getSignedFinalAnalysistablePerGroup(mainTable, "groupBelow", "durationBelow", 3, input$nFilter)   
      
      l<-list(dataTableAbove, dataTableBelow)
      mergedAboveBelow<-rbindlist(l, use.names=T, fill=T)
      
      g1<-CreateArrorChartsForRiseNRetrace(mergedAboveBelow)
      g1
    })
    
    output$Arrow_Value <- renderPlot({
      referencePoint<-as.numeric(input$RefValue)
      data<-getValueData()
      #data<-fread("D:/SHINYPROJECT/StrategyAnalysis/FinalPortfolioSeries.csv")
      g1<-CreateDensityCharts_ForValueNumbers(data[,MADist], 'Distribution of Values below and above SMA', 'Price ratio between T2-T3/T1-T2')
      g1
      #grid.arrange(g1,g2)
    })
    
    output$ExtreamValueTable_Pricetime_2 <- renderDataTable({
      print("Enter Prob Render Table")
      #       data<-fread("D:/SHINYPROJECT/StrategyAnalysis/FinalPortfolioSeries.csv")
      
      dataTableAbove<-getDataTableAbove()#getFinalAnalysistablePerGroup(mainTable, "groupAbove", "durationAbove", 2, isolate(input$nFilter))  
      dataTableBelow<-getDataTableBelow()#getFinalAnalysistablePerGroup(mainTable, "groupBelow", "durationBelow", 2, isolate(input$nFilter))   
      
      l<-list(dataTableAbove, dataTableBelow)
      mergedAboveBelow<-rbindlist(l, use.names=T, fill=T)
      
      TabletoPrint<-mergedAboveBelow[,.(TradeTime, T2_T1_PriceCng, T3_T2_PriceCng, T3_T1_PriceCng, T1_T2_Bars, T2_T3_Bars, T1_T3_Bars, SpeedTo_High, SpeedTo_End, SpeedTo_Start_End)]
      
      if (is.null(mergedAboveBelow)) {return()}
      TabletoPrint
    })
    
    ############################################################################## 
    
    output$ConfiTable_Above <- renderDataTable({
      print("Enter ConfiTable_Above Table")
      groupTable<-create4DAnalysisTable()
      #print(head(CalulateProfitInEach_BelowPattern_Loop(groupTable, 2, input$nMinBP, input$nMaxBP)))
      dataTableAbove=CalulateProfitInEach_AbovePattern_Loop(groupTable, 2, input$nMinBP, input$nMaxBP)
      print("Exit ConfiTable_Above Table")
      #print(head(dataTableAbove))
      dataTableAbove
    },
    options = list(lengthChange = FALSE)
    )

    output$ConfiTable_Below <- renderDataTable({
      print("Enter ConfiTable_Below Table")
      groupTable<-create4DAnalysisTable()
      #print(head(CalulateProfitInEach_BelowPattern_Loop(groupTable, 2, input$nMinBP, input$nMaxBP)))
      dataTableBelow=CalulateProfitInEach_BelowPattern_Loop(groupTable, 2, input$nMinBP, input$nMaxBP)
      print("Exit ConfiTable_Above Table")
      #print(head(dataTableBelow))
      dataTableBelow
    },
    options = list(lengthChange = FALSE)
    )
    
    output$PnL_Above<- renderTable({
      # groupTable<-create4DAnalysisTable()
      # dataTableAbove=CalulateProfitInEach_AbovePattern_Loop(groupTable, 2, input$nMinBP, input$nMaxBP)
      final.PnL.Above<- CalulateProfitInEach_AbovePattern_Reactive()
      ret <- sum(final.PnL.Above[,RoundedPnL])
      ret
    })
    
    output$PnL_ByBP_Above<- renderTable({
      final.PnL.Above<- CalulateProfitInEach_AbovePattern_Reactive()
      ret <- final.PnL.Above[,sum(RoundedPnL), by=RoundMADist]
      ret
    })
    
    output$PnL_Below<- renderTable({
      final.PnL.Below<- CalulateProfitInEach_BelowPattern_Reactive()
      ret <- sum(final.PnL.Below[,RoundedPnL])
      ret
    })
    
    output$PnL_ByBP_Below<- renderTable({
      final.PnL.Below<- CalulateProfitInEach_BelowPattern_Reactive()
      ret <- final.PnL.Below[,sum(RoundedPnL), by=RoundMADist]
      ret
    })
    
    output$Comparision_Speed__AboveCharts<- renderPlot({
      groupTable<-create4DAnalysisTable()
      dataTableAbove=CalulateProfitInEach_AbovePattern_Loop(groupTable, 2, input$nMinBP, input$nMaxBP)
      plotChartAbove(dataTableAbove)
    })
    
    output$Comparision_Speed__BelowCharts<- renderPlot({
      groupTable<-create4DAnalysisTable()
      dataTableBelow=CalulateProfitInEach_BelowPattern_Loop(groupTable, 2, input$nMinBP, input$nMaxBP)
      plotChartBelow(dataTableBelow)
    })
    
    output$CorrPlot<- renderPlot({
      print("Enter CorrPlot")
      #data<-fread("D:/SHINYPROJECT/StrategyAnalysis/FinalPortfolioSeries.csv")
      #data<-as.data.frame(getFreshData())
      nCols<-CalculateNumberOfInstruments(isolate(input$Inst1),isolate(input$Inst2),isolate(input$Inst3),isolate(input$Inst4),isolate(input$Inst5))
      #print(nCols)
      
      data<-as.data.frame(ntext())
#        print(head(data))
#       print(nrow(data))
#       print(ncol(data))
#       print(head(data[, -(1:as.numeric(nCols+7))]),1)
#       print(head(data[, -(1:as.numeric(nCols+7))][,1:as.numeric(nCols+1)]),2)
#       
#       print(cor(data[, -(1:as.numeric(nCols+7))][,1:as.numeric(nCols+1)]))
#       print("---------------")
      #data<-as.data.frame(getFreshData())
#       print(head(data))
#       print(head(data[, -(1:as.numeric(nCols+3))]))
#       print(head(data[, -(1:as.numeric(nCols+3))][,1:as.numeric(nCols+1)]))
#       print(nrow(data))
#       print(ncol(data))
#       print(cor(data[, -(1:as.numeric(nCols+3))][,1:as.numeric(nCols+1)]))
#       print(identical(data[, -(1:as.numeric(nCols+2))][,1:as.numeric(nCols+1)], data[, -(1:as.numeric(nCols+3))][,1:as.numeric(nCols+1)]))
#             
      #chart.Correlation(data[, -(1:as.numeric(nCols+4))], histogram = F, method = c("kendall"))
      corrData<-cor(data[, -(1:as.numeric(nCols+7))][,1:as.numeric(nCols+1)])
      #P-value 
      cor.mtest <- function(mat, conf.level = 0.95){
        mat <- as.matrix(mat)
        n <- ncol(mat)
        p.mat <- lowCI.mat <- uppCI.mat <- matrix(NA, n, n)
        diag(p.mat) <- 0
        diag(lowCI.mat) <- diag(uppCI.mat) <- 1
        for(i in 1:(n-1)){
          for(j in (i+1):n){
            tmp <- cor.test(mat[,i], mat[,j], conf.level = conf.level)
            p.mat[i,j] <- p.mat[j,i] <- tmp$p.value
            lowCI.mat[i,j] <- lowCI.mat[j,i] <- tmp$conf.int[1]
            uppCI.mat[i,j] <- uppCI.mat[j,i] <- tmp$conf.int[2]
          }
        }
        return(list(p.mat, lowCI.mat, uppCI.mat))
      }
      
      #res1 <- cor.mtest(corrData,0.95)
      print(corrData)
      corrplot(corrData*100,  method="circle",type="upper", diag=F, tl.pos="td", order="original", is.corr=F,
               #p.mat = res1[[1]], sig.level=0.05,
               cl.pos="b",addCoef.col="black",  tl.col="darkblue", tl.srt=45)#original
      #print("Exit CorrPlot")
    })

    observe({
      x<-NULL
      if(input$rdoIndexOrBond =="Bond Model"){
        x<-4
        updateNumericInput(session, "RefValue",
                           label = "Bond Ref Value(BP)",
                           value = x, step = .5)
        updateNumericInput(session, "ExtreamValue",
                           label = "Extream Value(BP)",
                           value = x, step = 1)
      }
      else {
        x<-4
        updateNumericInput(session, "RefValue",
                           label = "Index Ref Value(BP)",
                           #value = x, min = x-10, max = x+10, step = 5)
                           value = x,  step = .5)
        updateNumericInput(session, "ExtreamValue",
                           label = "Extream Value(BP)",
                           value = x, step = 1)
      }
      
    })

    output$PatternFoundSummary <- renderText({
      mainTable <- create4DAnalysisTable()
      maxFoundAbove <- mainTable[, max(groupAbove)]
      maxFoundBelow <- mainTable[, max(groupBelow)]
      maxDurationAbove <- mainTable[groupAbove > 0 , max(durationAbove)]#, by=groupAbove][, max(V1)]
      maxDurationBelow <- mainTable[groupBelow > 0 , max(durationBelow)]
      
      textAbove=paste("Total patterns found ABOVE SMA are", maxFoundAbove, "in which longest one took", maxDurationAbove, "bars of", isolate(input$nInterval), isolate(input$Interval))
      textBelow=paste("Total patterns found BELOW SMA are", maxFoundBelow, "in which longest one took", maxDurationBelow, "bars of", isolate(input$nInterval), isolate(input$Interval))
      paste(textAbove, "\n\n", textBelow)
    })

    output$MeanReversionBellCurve <- renderPlot({
      referencePoint<-as.numeric(input$RefValue)
      #print(referencePoint)
      #data<-ntext()
      #data<-fread("D:/SHINYPROJECT/StrategyAnalysis/FinalPortfolioSeries.csv")
      #mainTable=finalData
      #referencePoint=1

      dataTableAbove<-getDataTableAbove()#getFinalAnalysistablePerGroup(mainTable, "groupAbove", "durationAbove", 2, isolate(input$nFilter))   
      dataTableBelow<-getDataTableBelow()#getFinalAnalysistablePerGroup(mainTable, "groupBelow", "durationBelow", 2, isolate(input$nFilter))    
      
      g1<-CreateDensityCharts(dataTableAbove, dataTableBelow, 'More values > .5 the better Mean Reversion', 'Price ratio between T2-T3/T1-T2', .5, "P", input$StartFrom)
      #g2<-CreateDensityCharts(dataTableAbove, dataTableBelow, 'More values < 1 the better Mean Reversion', 'Time ratio between T2-T3/T1-T2', 1, "T")
      g1
      #grid.arrange(g1,g2)
    })

    output$PriceTime3_1 <- renderPlot({
      referencePoint<-as.numeric(input$RefValue)
      #print(referencePoint)
      #data<-ntext()
      #data<-fread("D:/SHINYPROJECT/StrategyAnalysis/FinalPortfolioSeries.csv")
      
      dataTableAbove<-getDataTableAbove()#getFinalAnalysistablePerGroup(mainTable, "groupAbove", "durationAbove", 2, isolate(input$nFilter))    
      dataTableBelow<-getDataTableBelow()#getFinalAnalysistablePerGroup(mainTable, "groupBelow", "durationBelow", 2, isolate(input$nFilter))  
      print("calculating main done")
      showAboveFilter<-as.numeric(input$nFilter)
      par(mfcol=c(2,1))
      calculateProbabilityofCycles(dataTableAbove, dataTableBelow, "T2_T1_PriceCng", referencePoint,showAboveFilter, 'Price Change from P1-P2', 'Price Change per Cycle', 'Peek Above MA', 'Peek Below MA')
      calculateProbabilityofCycles(dataTableAbove, dataTableBelow, "T3_T2_PriceCng", referencePoint,showAboveFilter, 'Price Change from P2-P3', 'Price Change per Cycle', 'Retrace Above MA', 'Retrace Below MA')
      par(mfcol=c(1,1))
    
      print("Exit ProbPlot")
    })

    output$PriceTime3_2 <- renderPlot({
      referencePoint<-as.numeric(input$RefValue)
      #print(referencePoint)
      #data<-ntext()
      #data<-fread("D:/SHINYPROJECT/StrategyAnalysis/FinalPortfolioSeries.csv")
      dataTableAbove<-getDataTableAbove()#getFinalAnalysistablePerGroup(mainTable, "groupAbove", "durationAbove", 2, isolate(input$nFilter))   
      dataTableBelow<-getDataTableBelow()#getFinalAnalysistablePerGroup(mainTable, "groupBelow", "durationBelow", 2, isolate(input$nFilter))   
      showAboveFilter<-as.numeric(input$nFilter)
      par(mfcol=c(2,1))
      calculateProbabilityofCycles(dataTableAbove, dataTableBelow, "T1_T2_Bars", referencePoint,showAboveFilter,'Number of Bars from T1-T2', 'Number of Bars per Cycle', 'Peek Above MA', 'Peek Below MA')
      calculateProbabilityofCycles(dataTableAbove, dataTableBelow, "T2_T3_Bars", referencePoint,showAboveFilter,'Number of Bars from T2-T3', 'Number of Bars per Cycle', 'Retrace Above MA', 'Retrace Below MA')
      par(mfcol=c(1,1))
      
      print("Exit ProbPlot")
    })

    output$PriceTime3_2D_1 <- renderPlot({
      #data<-ntext()
      #data<-fread("D:/SHINYPROJECT/StrategyAnalysis/FinalPortfolioSeries.csv")
      dataTableAbove<-getDataTableAbove()#getFinalAnalysistablePerGroup(mainTable, "groupAbove", "durationAbove",2, isolate(input$nFilter))    
            
      #par(mfcol=c(2,1))   #dataTableAbove,  dataTableBelow, column, referencePoint, title,            xlab, legend1, legend2
      Create2dPriceComparisionCharts(dataTableAbove, "groupAbove")
      #Create2dPriceComparisionCharts(dataTableBelow, "groupBelow")
      #par(mfcol=c(1,1))
      
      print("Exit ProbPlot")
    })

    output$PriceTime3_2D_2 <- renderPlot({
      #data<-ntext()
      #data<-fread("D:/SHINYPROJECT/StrategyAnalysis/FinalPortfolioSeries.csv")
      dataTableBelow<-getDataTableBelow()#getFinalAnalysistablePerGroup(mainTable, "groupBelow", "durationBelow",2, isolate(input$nFilter))   
      
      #par(mfcol=c(2,1))   #dataTableAbove,  dataTableBelow, column, referencePoint, title,            xlab, legend1, legend2
      #Create2dPriceComparisionCharts(dataTableAbove, "groupAbove")
      Create2dPriceComparisionCharts(dataTableBelow, "groupBelow")
      #par(mfcol=c(1,1))
      
      print("Exit ProbPlot")
    })

    output$PriceTime4D <- renderPlot({
      #data<-ntext()
      #data<-fread("D:/SHINYPROJECT/StrategyAnalysis/FinalPortfolioSeries.csv")
      print("calculating main")
      mainTable <- create4DAnalysisTable()#CeateMainGroupTable(data)
      print("calculating main done")
      dataTableAbove<-getFinalAnalysistablePerGroup(mainTable, "groupAbove", "durationAbove",2, isolate(input$nFilter))    
      
      Create4dCharts(dataTableAbove, "groupAbove")
      print("Exit ProbPlot")
    })

    output$PriceTime4D_2 <- renderPlot({
      #data<-ntext()
      #data<-fread("D:/SHINYPROJECT/StrategyAnalysis/FinalPortfolioSeries.csv")
      print("calculating main")
      mainTable <- create4DAnalysisTable()#CeateMainGroupTable(data)
      print("calculating main done")
      
      dataTableBelow<-getFinalAnalysistablePerGroup(mainTable, "groupBelow", "durationBelow",2, isolate(input$nFilter))  
      Create4dCharts(dataTableBelow, "groupBelow")
      print("Exit ProbPlot")
    })

    output$TradeTable = renderDataTable({
      print("Print Bins")
      dataTableAbove<-getDataTableAbove()
      dataTableBelow<-getDataTableBelow()
      
      TradeTable<-calculateTradeProfit(dataTableAbove[, .(T2_T1_PriceCng, T3_T2_PriceCng)], dataTableBelow[,.(T2_T1_PriceCng, T3_T2_PriceCng)], 
                                       input$StartFrom, input$EveryBP, input$Offset, input$DollarPerPoint, input$MaxSize)
      TradeTable
    } , options = list(
      lengthChange = FALSE,
      paging = FALSE,
      searching=FALSE,
      autowidth = FALSE
    ))

    output$TradeResults = renderDataTable({
      print("Printing Trade Results")
      dataTableAbove<-getDataTableAbove()
      dataTableBelow<-getDataTableBelow()
      
      TradeTable<-calculateTradeProfit(dataTableAbove[, .(T2_T1_PriceCng, T3_T2_PriceCng)], dataTableBelow[,.(T2_T1_PriceCng, T3_T2_PriceCng)],
                                       input$StartFrom, input$EveryBP, input$Offset, input$DollarPerPoint,input$MaxSize)
      TotalResults<- as.data.table(TradeTable)
      print(TotalResults[,pnl_pct])
      print(na.omit(TotalResults[,pnl_pct]))
      sumPNL_PCT<-TradeTable[, {Total_Trades=.N;Total_ProfitPCT=sum(pnl_pct); Total_Loss=sum(pnl_lost_dollar); Total_ProfitRetrace=sum(pnl_Retrace); Total_DiffinBP=sum(diff_bp);
                                list(Total_ProfitPCT=round(Total_ProfitRetrace/abs(Total_Loss)*100,1), Total_Trades=Total_Trades,  Total_Loss=Total_Loss, Total_ProfitRetrace=Total_ProfitRetrace, Total_DiffinBP=Total_DiffinBP)}]
      sumPNL_PCT
    } , options = list(
      lengthChange = FALSE,
      paging = FALSE,
      searching=FALSE,
      autowidth = FALSE
    ))
    output$VolatilityNumber<- renderText({
      print("Enter VolatilityNumber")
      #      data<-fread("D:/SHINYPROJECT/StrategyAnalysis/FinalPortfolioSeries.csv")
      data<-ntext()
      x<-data[,Spread]
      pop.var <- function(x) var(x) * (length(x)-1) / length(x)
      pop.sd <- function(x) sqrt(pop.var(x))
      sdt<-pop.sd(data[,Spread])
      sd2 <- sqrt(sum((x - mean(x))^2) / (length(x) - 1))
      #   print(sd(data[,Spread]))
      print("Exit VolatilityNumber")
      #     sd(data[,Spread])
      return(sdt)
    })
    
    output$HalfLife<- renderText({
      print("Enter HalfLife")
      #     data<-fread("D:/SHINYPROJECT/StrategyAnalysis/FinalPortfolioSeries.csv")
      data<-ntext()
      y <- data[,Spread]
      y.lag <- lag(abs(y), 1)
      delta.y <- diff(y)
      
      df <- as.data.frame(cbind(y, y.lag, delta.y))
      df <- df[-1 ,] #remove first row with NAs
      
      regress.results <- lm(delta.y ~ y.lag, data = df)
      
      lambda <- summary(regress.results)$coefficients[2]
      half.life <- -log(2)/lambda
      print("Exit HalfLife")
      round(half.life/2, 0)
    })
    
    output$HustExpo<- renderUI({
      # data<-fread("D:/SHINYPROJECT/StrategyAnalysis/FinalPortfolioSeries.csv")
      # Where H is the Hurst exponent, H serves as an indicator of the degree to which a series trends. For a trending series, H > 0.5, 
      # for a mean-reverting series, H < 0.5 and for a geometric random walk, H = 0.5
      # Hurst exponent
      data<-ntext()
      spread <- data[,Spread]
      hurst <- hurstexp((spread),display=F) # returns a list of various Hurst calculations
      THE<-paste("Theoretical Hurst exponent is ", round(as.numeric(hurst[5]),3))
      SRS<-paste("Simplified R over S approach is ", round(as.numeric(hurst[1]),3))
      HTML(paste( THE, SRS, sep='<br/>'))
    })
    
    output$HedgeRatio<- renderPlot({
      print("Enter Hedge Ratio")
      ## ordinary least squares regression
      #     data<-fread("D:/SHINYPROJECT/StrategyAnalysis/FinalPortfolioSeries.csv")
      data<-ntext()
      
      OLSBeta=getOLSHR(data$xtsData1.Close,data$xtsData2.Close)
      print(paste("OLSBeta",OLSBeta))
      TLSBeta=getTLSHR(data$xtsData1.Close,data$xtsData2.Close)
      print(paste("TLSBeta",TLSBeta))
      MVRBeta=getMVHR(data$xtsData1.Close,data$xtsData2.Close)
      print(paste("MVRBeta",MVRBeta))
      MVRCSBeta=getMVHR_CS(data[, get(input$Inst1)], data[, get(input$Inst2)], data$xtsData1.Close,data$xtsData2.Close, input$Inst1, input$Inst2)
      print(paste("MVRCSBeta",MVRCSBeta))
      
      ratio1<-as.numeric(isolate(input$Ratio1))
      ratio2<-as.numeric(isolate(input$Ratio2))
      
      #OriginalSpread<-na.omit(data[, ratio1 * xtsData1.Close + ratio2 * xtsData2.Close])
      #OLSSpread<-na.omit(data[, ratio1 * xtsData1.Close + ratio2 * OLSBeta * xtsData2.Close])
      #TLSSpread<-na.omit(data[, ratio1 * xtsData1.Close + ratio2 * TLSBeta * xtsData2.Close])
      #MVRSpread <- na.omit(data[, ratio1 * xtsData1.Close + ratio2 * MVRBeta * xtsData2.Close])
      #MVRCSpread <- na.omit(data[,ratio1 *  xtsData1.Close +ratio2 *  MVRCSBeta * xtsData2.Close])
      
      #       str1<-h5(paste("MVR Hedge ratio between ", inst1, " and ", inst2 , " is ", MVRBeta))
      #       str2<-h5(paste("MVRCS Hedge ratio between ", inst1, " and ", inst2 , " is ", MVRCSBeta))
      #       str3<-h5(paste("OLS Hedge ratio between ", inst1, " and ", inst2 , " is ", OLSBeta))
      #       str4<-h5(paste("TLS Hedge ratio between ", inst1, " and ", inst2 , " is ", TLSBeta))
      #       HTML(paste( str1, str2, str3, str4, sep='<br/>'))
      
      # par(mfcol=c(2,1))
      #plot(OriginalSpread, type="l", main=c("Original ", "SD ", round(sd(OriginalSpread),3)))
      #plot(OLSSpread, type="l", main=c("OLS ", round(OLSBeta,3),"SD ", round(sd(OLSSpread),3)))
      #plot(TLSSpread, type="l", main=c("TLS ", round(TLSBeta,3),"SD ", round(sd((TLSSpread)),3)))
      xtsInst1<-xts(data$xtsData1.Close, ymd_hms(data$TradeTime, tz="Asia/Kaula_Lumpur"))
      xtsInst2<-xts(data$xtsData2.Close, ymd_hms(data$TradeTime, tz="Asia/Kaula_Lumpur"))
      merge1<-cbind(xtsInst1,xtsInst2)  
      names(merge1)<-c("Inst1", "Inst2")
      
      OriginalSpread <- as.xts(merge1 %*% c(ratio1,ratio2), index(merge1))
      TLSSpread <- as.xts(merge1 %*% c(ratio1, ratio2 * TLSBeta), index(merge1))
      
      obj1 <- xyplot(OriginalSpread,col='blue')
      obj2 <- xyplot(TLSSpread,col='red')
      doubleYScale(obj1, obj2, add.axis = TRUE,style1 = 1, style2 = 1, text = c("Previous", "After"))
      #plot(spread, type="l", main=c("MVR ", round(MVRBeta,3),"SD ", round(sd(TLSSpread),3)))
      #plot(spread, type="l", main=c("MVR_CS", round(MVRCSBeta,3),"SD ", round(sd(spread),3)))
    })
    
    output$HedgeRatioValue<- renderText({
      print("Enter HedgeRatioValue")
      #     data<-fread("D:/SHINYPROJECT/StrategyAnalysis/FinalPortfolioSeries.csv")
      data<-ntext()
      TLSBeta_Price=getTLSHR(data$xtsData1.Close,data$xtsData2.Close)
      TLSBeta=getTLSHR(data[, get(input$Inst1)],data[,get(input$Inst2)])
      paste("<h4>Hedge Ratio   </h4><span style = 'color:blue; font-size:160%'>", 10,":", round(TLSBeta,2)*10,"  Price based " , round(TLSBeta_Price,3)*10, "</span>")
    })
    
    output$RegressionPlot<-renderPlot({
      data<-ntext()
      lme1<-lm(xtsData1.Close ~ xtsData2.Close, data)
      par(mfcol=c(2,2))
      plot(data$xtsData1.Close, data$xtsData2.Close, title='Relation between Two Inst.', xlab = 'Instrument 1', ylab = 'Instrument 2')
      abline(lme1, col='pink', lty=2)
      
      plot(fitted(lme1), residuals(lme1),  xlab = 'Fitted Values', ylab = 'Residuals')
      abline(h=0, lty=2)
      lines(smooth.spline(fitted(lme1), residuals(lme1)))
      plot(density(resid(lme1))) #A density plot
      qqnorm(resid(lme1)) # A quantile normal plot - good for checking normality
      #       qqline(resid(lme1))
      #       abline(reg, col="pink")
    })
    
    output$JohansonStats<-renderPrint({
      data<-ntext()
      data<-fread("D:/SHINYPROJECT/StrategyAnalysis/FinalPortfolioSeries.csv")
      xtsInst1<-xts(data$xtsData1.Close, ymd_hms(data$TradeTime, tz="Asia/Kaula_Lumpur"))
      xtsInst2<-xts(data$xtsData2.Close, ymd_hms(data$TradeTime, tz="Asia/Kaula_Lumpur"))
      merge1<-cbind(xtsInst1,xtsInst2) 
      names(merge1)<-c("ES", "Emini")
      jo_e <- ca.jo(merge1, type="eigen", ecdet="none", K=2) # eigenvalue test statistics
      results<-summary(jo_e)
      summ<-list()
      summ[["Test Stats"]]<-results@teststat
      summ[["Critical Values"]]<-results@cval
      summ[["Hedge Ratios"]]<-results@V[1:2]
      summ
      #       print(HTML(paste('<p>',var, '</p>')))
      #       invisible()
    })
    
    output$JohansonPlot<-renderPlot({
      data<-ntext()
      #data[, c("orgi", "new"):=list(xtsData1.Close-xtsData2.Close, xtsData1.Close- (0.501988 * xtsData2.Close))]
      xtsInst1<-xts(data$xtsData1.Close, ymd_hms(data$TradeTime, tz="Asia/Kaula_Lumpur"))
      xtsInst2<-xts(data$xtsData2.Close, ymd_hms(data$TradeTime, tz="Asia/Kaula_Lumpur"))
      merge1<-cbind(xtsInst1,xtsInst2)  
      names(merge1)<-c("ES", "Emini")
      jo_e <- ca.jo(merge1, type="eigen", ecdet="none", K=2) # eigenvalue test statistics
      results<-summary(jo_e)
      #       attributes(results) 
      coeffs<-vector()
      coeffs[1:2]<-results@V[1:2]
      cointegratedPair <- as.xts(merge1 %*% coeffs, index(merge1))
      originalPair <- as.xts(merge1 %*% c(1,-1), index(merge1))
      obj1 <- xyplot(originalPair, col='blue')
      obj2 <- xyplot(cointegratedPair,col='red')
      doubleYScale(obj1, obj2, add.axis = TRUE,style1 = 1, style2 = 1, text = c("Previous", "After"))
    })
    
    output$ADFTestStats<-renderPrint({
      data<-ntext()
      xtsInst1<-xts(data$xtsData1.Close, ymd_hms(data$TradeTime, tz="Asia/Kaula_Lumpur"))
      xtsInst2<-xts(data$xtsData2.Close, ymd_hms(data$TradeTime, tz="Asia/Kaula_Lumpur"))
      adf1<-ur.df(xtsInst1, type = "drift", lags = 1)
      results<-summary(adf1)
      #       attributes(results)
      summ<-list()
      summ[["Test Stats for Inst 1"]]<-results@teststat
      summ[["Critical Values"]]<-results@cval
      summ[["Hedge Ratios"]]<-results@V[1:2]
      
      adf2<-ur.df(xtsInst2, type = "drift", lags = 1)
      results2<-summary(adf2)
      summ<-list()
      summ[["Test Stats for Inst 2"]]<-results2@teststat
      summ[["Critical Values"]]<-results2@cval
      summ[["Hedge Ratios"]]<-results2@V[1:2]
      
      summ
      #       print(HTML(paste('<p>',var, '</p>')))
      #       invisible()
    })
    #HTML(paste0("Logged in as <code>", user()$user, "</code> with <code>", airlineName(userCompany()),"</code>."))
  })

  getJohnsonStats<-function(data){
    #       jo_t <- ca.jo(, type="trace", ecdet="none", K=2) #test trace statistics
    
  }
  getMVHR_CS <- function(p, q, inst1Price, inst2Price, inst1, inst2){
    corl<-cor(p,q)
    inst1SD<-sd(p)
    inst2SD<-sd(q)
    StatsHR<-corl*(inst1SD/inst2SD)
    print(StatsHR)
    
    Inst1LastPrice <- inst1Price[length(inst1Price)]
    Inst2LastPrice <- inst2Price[length(inst2Price)]
    Inst1TickValue <- getFullPointValue(inst1)
    Inst2TickValue <- getFullPointValue(inst2)
    Inst1ConrtactSize<-Inst1LastPrice * Inst1TickValue
    Inst2ConrtactSize<-Inst2LastPrice * Inst2TickValue
    dollarHR<- Inst1ConrtactSize/Inst2ConrtactSize
    print(dollarHR)
    
    finalHR <-StatsHR * dollarHR
    round(finalHR,3)
  }
  
  getMVHR <- function(p,q){
    corl<-cor(p,q)
    inst1SD<-sd(p)
    inst2SD<-sd(q)
    h<-corl*(inst1SD/inst2SD)
    round(h,3)
  }
  
  getOLSHR <- function(p,q){
    m<-lm(p~q)
    round(coef(m)[2],3)
  }
  
  getTLSHR <-function (p,q){
   # print(q)
    pca <- princomp(~ p + q)
    beta<-pca$loading[1,1]/pca$loading[2,1]
    round(beta,3)
  }

  calculateTradeProfit<- function(dataAbove, dataBelow, startFrom, every, offset, dollar_per_BP, maxSize){
    print("Calculating Trade Profit")
    #startFrom <- 2
    #every <- 1
    #offset <-.5
    #max_HL_Above <- floor(max(dataAbove[,T2_T1_PriceCng]) - offset)
    #actual_bp<-max_HL_Above-startFrom+1
    filteredTrades_Above<-dataAbove[T2_T1_PriceCng > startFrom, ]
    TradeData_above<-do.call(rbind,apply(filteredTrades_Above, 1, calculate_PL_perTrade, startFrom, offset, dollar_per_BP, maxSize))
    #print(TradeData_above)
    #max_HL_Below <- floor(max(dataBelow[,T2_T1_PriceCng]) - offset)
    #actual_bp<-max_HL_Above-startFrom+1
    filteredTrades_Below<-dataBelow[T2_T1_PriceCng > startFrom, ]
    TradeData_below<-do.call(rbind, apply(filteredTrades_Below, 1, calculate_PL_perTrade, startFrom, offset ,dollar_per_BP, maxSize))
    #print(TradeData_below)
    finalTradeTable <- rbind(TradeData_above, TradeData_below)
    class(finalTradeTable)
    finalTradeTable
  }
   
  calculate_PL_perTrade <- function(filteredTrades, startFrom, offset, dollar_per_BP, maxSize){
    print("")
    print(filteredTrades)
    print("Print calculate_PL_perTrade")
    #print(filteredTrades["T2_T1_PriceCng"])
    #print(floor(filteredTrades["T2_T1_PriceCng"] - startFrom))
    T2 <-filteredTrades["T2_T1_PriceCng"]
    bp_AwayfromStart <- floor(T2 - offset - startFrom)
    bp_AwayfromStart_Max <- ifelse(bp_AwayfromStart >= maxSize, maxSize, bp_AwayfromStart) #limiting the loss by max size
    total_lots_Short<-bp_AwayfromStart_Max + 1
    total_BP_Lost <- (bp_AwayfromStart_Max * (bp_AwayfromStart_Max+1))/2
    print(paste("T2",T2))    
    print(paste("bp_AwayfromStart", bp_AwayfromStart))
    print(paste("bp_AwayfromStart_Max", bp_AwayfromStart_Max))
    print(paste("total_lots_Short", total_lots_Short))
    print(paste("total_BP_Lost", total_BP_Lost))
    
    T3 <- filteredTrades["T3_T2_PriceCng"] 
    bp_AwayfromMax <- floor(ifelse((T3 - offset) > total_lots_Short, total_lots_Short, (T3 - offset))) # retacement point made cannot be more then n
    bp_AwayfromMax <- ifelse(total_lots_Short >= (maxSize + (2* offset)), 0, bp_AwayfromMax) # max the retrace to 0 if n croos max size and take a loss
    bp_AwayfromMax<-ifelse(bp_AwayfromMax < 0, 0, bp_AwayfromMax)
    total_BP_Retrace <- 0
    total_BP_Lost_ByLots<-0
    if(total_lots_Short > 0){
      total_BP_Lost_ByLots <- (total_lots_Short * (total_lots_Short+1))/2
      total_BP_Retrace <- total_BP_Lost_ByLots - ((total_lots_Short-bp_AwayfromMax) * ((total_lots_Short-bp_AwayfromMax)+1))/2
    
    }
    print(paste("T3",T3))
    print(paste("bp_AwayfromMax", bp_AwayfromMax))
    print(paste("total_BP_Retrace",total_BP_Retrace))
    print(paste("total_BP_Lost_ByLots",total_BP_Lost_ByLots))
    pnl_Retrace<-total_BP_Retrace * dollar_per_BP
    diff_bp<-total_BP_Retrace-total_BP_Lost_ByLots
    pnl_lost_dollar <- diff_bp * dollar_per_BP
    pnl_pct<-0
    if(total_BP_Lost>0)
      pnl_pct = round(total_BP_Retrace/total_BP_Lost * 100,0)
    #dtTrade <- data.table({T2=n, Dist_SMA_T2=final_max_n, T3=m, Dist_fromSMA=final_m, startFrom, Total_BP_T2=sumBP_HL, Total_BP_T3=total_BP_Retrace,  DOllar_Retrace=pnl_Retrace, Diff_BP=diff_bp, Dollar_Lost=pnl_lost_dollar, Profit_Pct=pnl_pct})
    dtTrade <- data.table(T2, bp_AwayfromStart_Max, total_lots_Short,total_BP_Lost, total_BP_Lost_ByLots, startFrom, T3, bp_AwayfromMax,  total_BP_Retrace,  pnl_Retrace, diff_bp, pnl_lost_dollar, pnl_pct)
    print(dtTrade)
    dtTrade[total_lots_Short>0,]
  }
    
  calculateBins<- function(dataAbove, dataBelow, normalize, binsBy){
    print("Calculating Bins")
    #binsBy <- 10
    #normalize <- 1
    x<-round(as.numeric(dataAbove)/normalize,1)
    print(paste(normalize, binsBy))
    aboveBins<-as.data.table(table(cut(x, breaks=seq(min(floor(x)), max(ceiling(x)), by=binsBy))))
    #setnames(aboveBins, c("V1", "N"), c("Basis Point(Above SMA)", "Counts"))
    print("aboveBins")
    print(aboveBins)
    
    y<-round(as.numeric(dataBelow)/normalize,1)
    belowBins<-as.data.table(table(cut(y, breaks=seq(min(floor(y)), max(ceiling(y)), by=binsBy))))
    #setnames(belowBins, c("V1", "N"), c("Basis Points(Above SMA)", "Counts"))
    print("belowBins")
    print(belowBins)
    finalTable <- merge(aboveBins,belowBins,by="V1",all=T, sort = F)
    setnames(finalTable, c("V1", "N.x", "N.y"), c("Basis Points", "Counts (Above SMA)", "Counts (Below SMA)"))
    print(finalTable)
    finalTable
  }

  calculateSummaryPlotOld <- function(data, sma){
   # data<-fread("D:/SHINYPROJECT/StrategyAnalysis/FinalPortfolioSeries.csv")
    data<-as.data.table(data)
    Price<-data[,Spread]
    if(length(Price) < sma)
      stop(paste('Oops! Only',  length(Price), 'number of rows. Cannot calculate SMA.'))
  
    PositiveDT<-data[MADist>0,MADist]
    NegetiveDT<-data[MADist<0,-MADist]
    
    AboveSMA<-summary(fivenum(PositiveDT))
    BelowSMA<-summary(fivenum(NegetiveDT))
    
    finaldata<-rbind(AboveSMA, BelowSMA)
    #boxplot(t(finaldata), horizontal = T)
    #print(summary(fivenum(PositiveDT)))
    return(finaldata)
  }
  
  calculateSummaryPlot <- function(dataAbove, dataBelow){
    # data<-fread("D:/SHINYPROJECT/StrategyAnalysis/FinalPortfolioSeries.csv")
   # data<-as.data.table(data)
    #Price<-data[,Spread]
    #if(length(Price) < sma)
    #  stop(paste('Oops! Only',  length(Price), 'number of rows. Cannot calculate SMA.'))
    
    PositiveDT<-dataAbove
    NegetiveDT<-dataBelow
    
    AboveSMA<-summary(fivenum(PositiveDT))
    BelowSMA<-summary(fivenum(NegetiveDT))
    
    finaldata<-rbind(AboveSMA, BelowSMA)
    #boxplot(t(finaldata), horizontal = T)
    #print(summary(fivenum(PositiveDT)))
    return(finaldata)
  }
  
  calculateSummaryAbove <- function(dataAbove){
  #    print("calculateSummaryAbove")
    PositiveDT<-dataAbove#data[MADist>0,MADist]
  #   print(summary(fivenum(PositiveDT)))
    summaryAbove<-summary(fivenum(PositiveDT))
    #summaryAbove<-boxplot.stats(PositiveDT)$stats
    #names(summaryAbove) <- c("Min", "25%", "Median", "75%", "Max")
    #summaryAbove[["Mean"]]<-round(sum(PositiveDT)/length(PositiveDT),4)
    summaryAbove[["SD"]]<-round(sd(PositiveDT),4)
    summaryAbove[["N"]]<-length(PositiveDT)
    return(round(summaryAbove,4))
  }
  
  calculateSummaryBelow <- function(dataBelow){
#   print("calculateSummaryBelow")
#   data<-as.data.table(data)
  NegetiveDT<-dataBelow#data[MADist<0,-MADist]
  summaryBelow<-summary(fivenum(NegetiveDT))
  summaryBelow[["SD"]]<-round(sd(NegetiveDT),4)
  summaryBelow[["N"]]<-length(NegetiveDT)
  return(round(summaryBelow,4))
}

  Calculate_Vaue_Probability <- function(valueData,referencePoint, title, xlab, sma){
  print('enter calculateProbability')
  
  #data<-fread("D:/SHINYPROJECT/StrategyAnalysis/FinalPortfolioSeries.csv")
  #referencePoint<-4
  #title="Value prob plot (Virtical distance)"
  #xlab="Basis Points"
  
  names<-colnames(valueData)
  if(!("MADist" %in% names))
    stop("Not sufficient data to calculate the moving avarage.")
  # filteredData<-valueData[(MADist > thresholdVal) & (MADist<-thresholdVal) ,MADist]
  AboveDT<-valueData[MADist > 0,MADist]
  BelowDT<-valueData[MADist < 0,-MADist]
  #summary(fivenum(NegativeDT))
  
  above.ecdf<-ecdf(AboveDT)
  below.ecdf<-ecdf(BelowDT)
  
  ############################################## Find Probalility
  acumulated.distrib= function(sample,x){
    minors= 0
    for(n in sample){
      
      if(n<=x){
        minors= minors+1
      }
    }
    return (minors/length(sample))
  }
  
  #referencePoint<-input$Value # make variable
  
  ################################### Plot Price above MA
  aHistColor=rgb(0,0,0,00)
  plot.ecdf(AboveDT)
  # create the plot directly with just the plot.ecdf() function, but this doesn't produce any empirical CDF values
  plot.ecdf(AboveDT,xlab = xlab, ylab = '',bg="black",  main = title, col=aHistColor,do.points = F, lwd =1)
  xx <- unique(sort(c(seq(-3, 2, length = 201), knots(above.ecdf))))
  lines(xx, above.ecdf(xx), col = "red", lwd=3)
  # mtext(text = expression(hat(F)[n](x)), side = 2, line = 2)
  mtext(text = "Probability", side = 2, line = 2)
  percetage<-acumulated.distrib(na.omit(AboveDT),referencePoint)
  abline(v = referencePoint, h = percetage)
  #legend(referencePoint,percetage , paste0(round(percetage * 100,1), '%', ' Prob of <= ', referencePoint ) , box.lwd = 0)
  legend(referencePoint,percetage +.1 , paste0(round(percetage * 100,1), '%') , box.lwd = 0, text.col = "red", cex = 1.5, bty = "n")
  
  ################################### Plot Price below MA
  
  bHistColor=rgb(0,0,0,0)
  plot.ecdf(BelowDT, ylab = '',  , col=bHistColor, do.points = F, lwd =1, pch = 1,  add=T)
  xx <- unique(sort(c(seq(-3, 2, length = 201), knots(below.ecdf))))
  lines(xx, below.ecdf(xx), col = "darkgreen", lwd=3)
  percetage<-acumulated.distrib(na.omit(BelowDT),referencePoint)
  abline(v = referencePoint, h = percetage)
  #legend(referencePoint,percetage , paste0(round(percetage * 100,1), '%', ' Prob of <= ', referencePoint ) , box.lwd = 0)
  legend(referencePoint,percetage , paste0(round(percetage * 100,1), '%') , box.lwd = 0, text.col = "darkgreen",  cex = 1.5, bty = "n")
  #legend('right', c(legend1, legend2), fill=c('red', 'darkgreen'), adj = c(0, 0.5), text.width =8)
  print("exit calculateProbability")
}

  
#shiny::runApp('D:/SHINYPROJECT/StrategyAnalysis',host="192.168.115.234",port=3700)

# shinyapps::deployApp("D:/SHINYPROJECT/StrategyAnalysis", account='prashantapp')
#rsconnect::setAccountInfo(name='prashantapp',token='2ED7C26847BF512045DC63E28B3D2348',secret='f8mwylqwKoFfiLN6NQA1n+4JpN7WIK0VqWfXwsRD')
