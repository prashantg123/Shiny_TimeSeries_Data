
require(gridExtra)
library(plot3D)
library(doParallel)#library(foreach)
library(doSNOW)
library(foreach)

CreatePartitiongGroupTable <- function(dataPartition, lookbackYear, byEvery, lookbackDaysPast, lookbackDaysFuture, referenceDate){
print("Enter CreatePartitiongGroupTable")
  dataPartition<-fread("D:/SHINYPROJECT/StrategyAnalysis/FinalPortfolioSeries.csv")
  # lookbackYear=5
  # byEvery="-1 month"
  # lookbackDaysPast =-5
  # lookbackDaysFuture =5
  # referenceDate <-ymd('2016-06-23')
#print(head(data))
 print(lookbackYear)
 print(byEvery)
 print(lookbackDaysPast)
 print(lookbackDaysFuture)
 print(referenceDate)
  refDateSeries <- seq(referenceDate, by=byEvery, length.out=lookbackYear)[2:lookbackYear]
  startRefDateSeries <- refDateSeries + days(lookbackDaysPast)
  endRefDateSeries <- refDateSeries + days(lookbackDaysFuture)
  print(startRefDateSeries)
  print(refDateSeries)
  print(endRefDateSeries)
  # referenceDate <- ymd('2016-07-16')
  
  groupedData<-NULL
  groupNumber=0
  #data[, group:=NA]
  #data[, subGroup:=NA]
  
  dataPartition[, Change:=0]
  write.csv(data, "D:/SHINYPROJECT/StrategyAnalysis/PREtestdata.csv")
  for (i in seq(1:length(refDateSeries)))
  {
    #i=4
    groupNumber = groupNumber + 1
    iStartDate <- startRefDateSeries[i]
    iMidDate <- refDateSeries[i]
    iEndDate <- endRefDateSeries[i]
    print(i)
    print(iStartDate)
    print(iMidDate)
    print(iEndDate)
    dataPartition[as.Date(TradeTime) >= iStartDate & as.Date(TradeTime) <= iMidDate, c("group", "subGroup") :=.(format(iMidDate, "%Y-%m"),"Before")]
    dataPartition[as.Date(TradeTime) > iMidDate & as.Date(TradeTime) <= iEndDate, c("group", "subGroup") :=.(format(iMidDate, "%Y-%m"),"After")]
  }
  write.csv(dataPartition, "D:/SHINYPROJECT/StrategyAnalysis/testdata.csv")
  dataPartition[, Change := round(Spread - first(Spread),3), by=list(group,subGroup) ]
  groupedData <- dataPartition[!is.na(group)]
  groupedData[, V1:=1:.N]
  write.csv(groupedData, "D:/SHINYPROJECT/StrategyAnalysis/PartitionDataWithGroups.csv")
  #print(groupedData)
  print("finifsh group")
  return(groupedData)
}
    

CeateMainGroupTable <- function(data){
  #data<-fread("D:/SHINYPROJECT/MADistanceModel/FinalPortfolioSeries.csv")
  
  names<-colnames(data)
  if(!("MADist" %in% names))
    stop("Not sufficient data to calculate the moving avarage.")
  #print(data)
  table<-na.omit(data[, .(TradeTime, Spread, SpreadRet, Indicator, MADist) ])
  totalRows <- nrow(table)
  table[, RoundMADist:=floor(abs(MADist)) * sign(MADist)]
  table[, oneAbovePrice:= ifelse(Spread >= Indicator, 1,0)]
  table[, durationAbove:=0]
  table[, groupAbove:=-1]
  table[, Cross_Price:=0]
  table[, Value_Point:=0]
  incProgress(.2, detail="Finding upper patterns")
  groupNumber =1
  print("Finding upper patterns")
  for (i in 2:as.numeric(totalRows)){
   # i=2
    if (table[i, oneAbovePrice] ==1 & table[i-1, oneAbovePrice] ==0){
      table[i, durationAbove:=1]
      table[i, groupAbove:=groupNumber]
      table[i, Cross_Price:=table$Spread[i]]
      groupNumber = groupNumber+1
    } else if (table[i, oneAbovePrice] ==1) {
      table[i, durationAbove:=table$durationAbove[i-1]+1]
      table[i, groupAbove:=table$groupAbove[i-1]]
      table[i, Cross_Price:=table$Cross_Price[i-1]]
    }
  }
  incProgress(.5, detail="Finding lower patterns")
  print("Finding Lower patterns")
  table[, oneBelowPrice:= ifelse(Spread <= Indicator, 1,0)]
  table[, durationBelow:=0]
  table[, groupBelow:=-1]
  groupNumber =1
  for (i in 2:as.numeric(totalRows)){
    if (table[i, oneBelowPrice] ==1 & table[i-1, oneBelowPrice] ==0){
      table[i, durationBelow:=1]
      table[i, groupBelow:=groupNumber]
      table[i, Cross_Price:=table$Spread[i]]
      groupNumber =groupNumber+1
    } else if (table[i, oneBelowPrice] ==1) {
      table[i, durationBelow:=table$durationBelow[i-1]+1]
      table[i, groupBelow:=table$groupBelow[i-1]]
      table[i, Cross_Price:=table$Cross_Price[i-1]]
    }
  }
  incProgress(.3, detail="Finished finiding patterns")
  table[, Speed:=round(Spread-Cross_Price,3)]
  table[oneBelowPrice==1, Confi_Num:=round(Speed*MADist/durationBelow,3)]
  table[oneAbovePrice==1, Confi_Num:=round(Speed*MADist/durationAbove,3)]
  
  print("Finished finiding patterns")
  #write.csv(table,"D:/SHINYPROJECT/StrategyAnalysis/HVAnalysis.csv")
  table
}

getFinalAnalysistablePerGroup  <- function(groupTable, whichGroup, whichDuration, minBarforPatterns, min_T1_T2_Change){
  #initial Price and Max Duration
  #withProgress(message = 'Getting Final Analysis table per Group...', detail = "Please wait till finish...", value = 0, { 
  print(paste("Enter getFinalAnalysistablePerGroup_", whichGroup))
    # groupTable=fread("D:/SHINYPROJECT/StrategyAnalysis/HVAnalysis.csv")
    # whichGroup="groupAbove"
    # whichDuration="durationAbove"
    # minBarforPatterns=2
    # min_T1_T2_Change=.1
  incProgress(.2)
  InitialPriceperGroup <- list()
  InitialPriceperGroup <- groupTable[, list(first(TradeTime), MaxDuration=.N, first(Spread)), by=get(whichGroup)]
  #mainTable, "groupAbove", "durationAbove", 2
  #Initial Duration
  InitialPriceperGroup[, StartDuration:=1]
  names(InitialPriceperGroup)[1]<- whichGroup
  
  #High Price
  PriceHighperGroup<- list()
  if(whichGroup=="groupAbove")
    PriceHighperGroup <- groupTable[, max(Spread), by=get(whichGroup)]  # max for above group
  if(whichGroup=="groupBelow")
    PriceHighperGroup <- groupTable[, min(Spread), by=get(whichGroup)]  # min for below group
  names(PriceHighperGroup)[1]<- whichGroup
  
  analysisTable<-data.table()
  analysisTable<-merge(InitialPriceperGroup, PriceHighperGroup, by=whichGroup)
  
  setnames(analysisTable, c("V1.x", "MaxDuration", "V3", "StartDuration", "V1.y"), c("StartTime", "T1_T3_Bars", "T1_Price", "T0_Bar", "T2_Price"))
  #write.csv(analysisTable, paste("D:/SHINYPROJECT/StrategyAnalysis/Analysis1_", whichGroup, ".csv"))
  incProgress(.2)
  # Duration at high Price
  DurationTillMax <- list()
  if(whichGroup=="groupAbove")
    DurationTillMax<- as.data.table(groupTable[, .SD[which.max(Spread)], by=get(whichGroup)][,get(whichDuration)])
  if(whichGroup=="groupBelow")
    DurationTillMax<- as.data.table(groupTable[, .SD[which.min(Spread)], by=get(whichGroup)][,get(whichDuration)])
  analysisTable<-analysisTable[, DurationTillMax:=DurationTillMax]#[DurationTillMax > 0,] #"T1_T2_Bars > 0 condition
  incProgress(.5)
  #higherst distance from SMA
  MaxDistance_FromMA <-list()
  if(whichGroup=="groupAbove")
    MaxDistance_FromMA<- as.data.table(groupTable[, max(MADist), by=get(whichGroup)][,V1])
  if(whichGroup=="groupBelow")
    MaxDistance_FromMA<- as.data.table(groupTable[, min(MADist), by=get(whichGroup)][,V1])
  analysisTable<-analysisTable[, MaxDistance_FromMA:=MaxDistance_FromMA]#[MaxDistance_FromMA > 0,] #"T1_T2_Bars > 0 condition
  
  analysisTable<-analysisTable[, MaxDistance_FromMA_Rounded:=floor(abs(MaxDistance_FromMA))]
  
  # Price At End
  PriceAtEnd <- list()
  PriceAtEnd<-groupTable[, last(Spread), by=get(whichGroup)]
  
  names(PriceAtEnd)[1]<- whichGroup
  #write.csv(PriceAtEnd, paste("D:/SHINYPROJECT/StrategyAnalysis/Analysis2_", whichGroup , ".csv"))
  
  analysisTable<-merge(analysisTable, PriceAtEnd, by=whichGroup)
  incProgress(.2)
  #Duration Max to End
  analysisTable[, DurationMaxToEnd:=T1_T3_Bars-DurationTillMax]
  incProgress(.3)
  setnames(analysisTable, c("DurationTillMax","V1", "DurationMaxToEnd"), c("T1_T2_Bars", "T3_price", "T2_T3_Bars"))
  # Calcualte Price Change
  analysisTable[, T2_T1_PriceCng:=round(abs(T2_Price - T1_Price),4)][, T3_T2_PriceCng:=round(abs(T3_price - T2_Price),4)][, T3_T1_PriceCng:=round(abs(T3_price - T1_Price),4)]
  #Calculate ratio change between price and time
  analysisTable[, PeekRetace_PriceRatio:=round(T3_T2_PriceCng/T2_T1_PriceCng, 4)][, PeekRetace_TimeRatio:=round(T2_T3_Bars/T1_T2_Bars, 4)]
  #Calculate speed
  
  analysisTable[, SpeedTo_High:=round(T2_T1_PriceCng/T1_T2_Bars, 4)][, SpeedTo_End:=round(T3_T2_PriceCng/T2_T3_Bars, 4)][, SpeedTo_Start_End:=round(T3_T1_PriceCng/T1_T3_Bars, 4)]
  #filter small groups
  incProgress(.1)
  analysisTable<-analysisTable[get(whichGroup) > 0 & T1_T3_Bars > minBarforPatterns & T2_T3_Bars > 0 & T1_T2_Bars > 0 & T3_T2_PriceCng > 0 & T2_T1_PriceCng > as.numeric(min_T1_T2_Change)]
  #write.csv(analysisTable,paste0("D:/SHINYPROJECT/StrategyAnalysis/analysisTable_", whichGroup, ".csv"))
  print(paste("Exit getFinalAnalysistablePerGroup_", whichGroup))
  analysisTable
  #})
}

GetProfitInEachPattern <- function(groupNumber, groupTable, whichGroup, minBarforPatterns, startingMADist, endingMADist){
  # whichGroup="groupAbove"
  #  groupNumber=1
  # # whichGroup="groupBelow"
  # minBarforPatterns=2
  # startingMADist=2
  # endingMADist=12
  # 
  byEach_UniqueMADist <-NULL
  eachGroup=NULL
  eachGroup.aggregated=NULL
  #(eachGroup<-groupTable[groupBelow == groupNumber,][max(durationBelow) > minBarforPatterns, ] )
  if(whichGroup=="groupAbove"){
    #print(paste("In group Above : ", groupNumber))
    
    eachGroup<-groupTable[groupAbove == groupNumber,][max(durationAbove) > minBarforPatterns, ] 
    eachGroup<-eachGroup[, list("Price"=Spread, Indicator, RoundMADist, Speed,Confi_Num, durationAbove,groupAbove) ]
    eachGroup[,c("PL","StartPrice", "EndPrice", "RoundedPnL"):= .(0, 0, 0, 0)]
    eachGroup<-eachGroup[,.(groupAbove, StartPrice, Price, Indicator, RoundMADist, EndPrice, durationAbove, Speed,  Confi_Num, PL, RoundedPnL)]
    if(nrow(eachGroup)>0){
      eachGroup<-eachGroup[, list(groupAbove, "StartPrice"=first(Price), Price, Indicator, RoundMADist, "EndPrice"=last(Price), durationAbove, Speed,  Confi_Num)]
      eachGroup[,c("PL"):= .(ifelse(RoundMADist == 0, 0, EndPrice-Price))][,c("RoundedPnL"):= .(floor(abs(PL)) * sign(PL))]
    }
   # else {return(eachGroup) }
  } else if(whichGroup=="groupBelow") {
    #print(paste("In group Below : ", groupNumber))
    eachGroup<-groupTable[groupBelow == groupNumber,][max(durationBelow) > minBarforPatterns, ] 
    eachGroup<-eachGroup[, list("Price"=Spread, Indicator, RoundMADist, Speed,Confi_Num, durationBelow,groupBelow) ]
    eachGroup[,c("PL","StartPrice", "EndPrice", "RoundedPnL"):= .(0, 0, 0, 0)]
    eachGroup<-eachGroup[,.(groupBelow, StartPrice, Price, Indicator, RoundMADist, EndPrice, durationBelow, Speed,  Confi_Num, PL, RoundedPnL)]
    if(nrow(eachGroup)>0){
      eachGroup<-eachGroup[, list(groupBelow, "StartPrice"=first(Price), Price, Indicator, RoundMADist, "EndPrice"=last(Price), durationBelow, Speed,  Confi_Num)]
      eachGroup[,c("PL"):= .(ifelse(RoundMADist == 0, 0, EndPrice-Price))][,c("RoundedPnL"):= .(floor(abs(PL)) * sign(PL))]
    }
   # else {return(eachGroup)}
  }
  
  #eachGroup[!duplicated(RoundMADist),][abs(RoundMADist) >= startingMADist][abs(RoundMADist) <= endingMADist]
  byEach_UniqueMADist <- eachGroup[!duplicated(RoundMADist),][abs(RoundMADist) >= startingMADist & abs(RoundMADist) <= endingMADist]
  byEach_UniqueMADist
}

CalulateProfitInEach_AbovePattern <- function(groupTable, minBarforPatterns, startingMADist, endingMADist){
  cl <- makePSOCKcluster(2)
  registerDoParallel(cl, cores=2)
  final.PnL.Above<-NULL
  minBarforPatterns=2
  final.PnL.Above<-foreach(i=1:length(unique(groupTable[groupAbove > 0,groupAbove])), .combine="rbind", .packages="data.table", .verbose=F) %dopar%  GetProfitInEachPattern(i, groupTable, "groupAbove", minBarforPatterns, startingMADist, endingMADist)
  stopCluster(cl)
  final.PnL.Above
}

CalulateProfitInEach_BelowPattern <- function(groupTable, minBarforPatterns, startingMADist, endingMADist){
  a1<-Sys.time()
  cl <- makePSOCKcluster(2)
  registerDoParallel(cl, cores=2)
  final.PnL.Below<-NULL
  minBarforPatterns=2
  final.PnL.Below<-foreach(i=1:length(unique(groupTable[groupBelow > 0,groupBelow])), .combine="rbind", .packages="data.table", .verbose=F) %dopar%  GetProfitInEachPattern(i, groupTable, "groupBelow", minBarforPatterns, startingMADist, endingMADist)
  #final.PnL.Below<-foreach(i=1:length(unique(groupTable[groupBelow > 0,groupBelow])), .combine="rbind", .packages="data.table", .verbose=F) %dopar%  pp()
  stopCluster(cl)
  Sys.time()-a1
  final.PnL.Below
}

CalulateProfitInEach_AbovePattern_Loop <- function(groupTable, minBarforPatterns, startingMADist, endingMADist){
  a1<-Sys.time()
  final.PnL.Above<-NULL
  #minBarforPatterns=2
  for(i in 1:length(unique(groupTable[groupAbove > 0,groupAbove]))){
    if(i==1) {
      final.PnL.Above<-GetProfitInEachPattern(i, groupTable, "groupAbove", minBarforPatterns, startingMADist, endingMADist)
    } else {
      print(head(GetProfitInEachPattern(i, groupTable, "groupAbove", minBarforPatterns, startingMADist, endingMADist)))
      final.PnL.Above<-rbind(final.PnL.Above, GetProfitInEachPattern(i, groupTable, "groupAbove", minBarforPatterns, startingMADist, endingMADist))
    }
  }
  Sys.time()-a1
  # print(head(final.PnL.Above))
  final.PnL.Above
}

CalulateProfitInEach_BelowPattern_Loop <- function(groupTable, minBarforPatterns, startingMADist, endingMADist){
  a1<-Sys.time()
  final.PnL.Below<-NULL
  #minBarforPatterns=2
  for(i in 1:length(unique(groupTable[groupBelow > 0,groupBelow]))){
    if(i==1) {
      #print(GetProfitInEachPattern(i, groupTable, "groupBelow", minBarforPatterns, startingMADist, endingMADist))
      final.PnL.Below<-GetProfitInEachPattern(i, groupTable, "groupBelow", minBarforPatterns, startingMADist, endingMADist)
    } else {
     # print(head(GetProfitInEachPattern(i, groupTable, "groupBelow", minBarforPatterns, startingMADist, endingMADist)))
      final.PnL.Below<-rbind(final.PnL.Below, GetProfitInEachPattern(i, groupTable, "groupBelow", minBarforPatterns, startingMADist, endingMADist))
    }
  }
  Sys.time()-a1
  # print(head(final.PnL.Below))
  final.PnL.Below
}

# GetProfitInEachPattern(4, groupTable, "groupBelow", 2,2,6)
# 
# a<-CalulateProfitInEach_AbovePattern_Loop(groupTable, 2, 2, 12)
# plotChartAbove()
# b<-CalulateProfitInEach_BelowPattern_Loop(groupTable, 2, 2, 12)
# plotChartBelow()

plotChartBelow<-function(final.PnL.Below){
  par(mfrow=c(2,2))
  plot(final.PnL.Below[, RoundedPnL], final.PnL.Below[, RoundMADist])
  plot(final.PnL.Below[, RoundedPnL], final.PnL.Below[, durationBelow])
  plot(final.PnL.Below[, RoundedPnL], final.PnL.Below[, Confi_Num])
  plot(final.PnL.Below[, RoundedPnL], final.PnL.Below[, Speed])
  par(mfrow=c(1,1))
}

plotChartAbove<-function(final.PnL.Above){
  par(mfrow=c(2,2))
  plot(final.PnL.Above[, RoundedPnL], final.PnL.Above[, RoundMADist])
  plot(final.PnL.Above[, RoundedPnL], final.PnL.Above[, durationAbove])
  plot(final.PnL.Above[, RoundedPnL], final.PnL.Above[, Confi_Num])
  plot(final.PnL.Above[, RoundedPnL], final.PnL.Above[, Speed])
  par(mfrow=c(1,1))
}

AggregatedPnLTable_Above<-function(final.PnL.Above){
  displayList<-list()

  displayList[[1]] <- sum(final.PnL.Above[,RoundedPnL])
  displayList[[2]] <- final.PnL.Above[,sum(RoundedPnL), by=RoundMADist]
  displayList
}

AggregatedPnLTable_Below<-function(final.PnL.Below){
  # sum(final.PnL.Below[,RoundedPnL])
  # final.PnL.Below[,sum(RoundedPnL), by=RoundMADist]
  
  displayList[[1]] <- sum(final.PnL.Above[,RoundedPnL])
  displayList[[2]] <- final.PnL.Above[,sum(RoundedPnL), by=RoundMADist]
  displayList
}
#CalulateProfitInEachPattern(155,groupTable, "groupAbove")
#CalulateProfitInEachPattern(14,groupTable, "groupBelow")
#############################################################################################################


getSignedFinalAnalysistablePerGroup <- function(groupTable, whichGroup, whichDuration, minBarforPatterns, min_T1_T2_Change){
  #initial Price and Max Duration
  InitialPriceperGroup <- list()
  InitialPriceperGroup <- groupTable[, list(first(TradeTime), MaxDuration=.N, first(Spread)), by=get(whichGroup)]
  #mainTable, "groupAbove", "durationAbove", 2
  #Initial Duration
  InitialPriceperGroup[, StartDuration:=1]
  names(InitialPriceperGroup)[1]<- whichGroup
  
  #High Price
  PriceHighperGroup<- list()
  if(whichGroup=="groupAbove")
    PriceHighperGroup <- groupTable[, max(Spread), by=get(whichGroup)]  # max for above group
  if(whichGroup=="groupBelow")
    PriceHighperGroup <- groupTable[, min(Spread), by=get(whichGroup)]  # min for below group
  names(PriceHighperGroup)[1]<- whichGroup
  
  analysisTable<-data.table()
  analysisTable<-merge(InitialPriceperGroup, PriceHighperGroup, by=whichGroup)
  
  setnames(analysisTable, c("V1.x", "MaxDuration", "V3", "StartDuration", "V1.y"), c("StartTime", "T1_T3_Bars", "T1_Price", "T0_Bar", "T2_Price"))
  
  # Duration at high Price
  DurationTillMax <- list()
  if(whichGroup=="groupAbove")
    DurationTillMax<- as.data.table(groupTable[, .SD[which.max(Spread)], by=get(whichGroup)][,get(whichDuration)])
  if(whichGroup=="groupBelow")
    DurationTillMax<- as.data.table(groupTable[, .SD[which.min(Spread)], by=get(whichGroup)][,get(whichDuration)])
  analysisTable[, DurationTillMax:=DurationTillMax]
  
  #higherst distance from SMA
  MaxDistance_FromMA <-list()
  if(whichGroup=="groupAbove")
    MaxDistance_FromMA<- as.data.table(groupTable[, max(MADist), by=get(whichGroup)][,V1])
  if(whichGroup=="groupBelow")
    MaxDistance_FromMA<- as.data.table(groupTable[, min(MADist), by=get(whichGroup)][,V1])
  analysisTable<-analysisTable[, MaxDistance_FromMA:=MaxDistance_FromMA]#[MaxDistance_FromMA > 0,] #"T1_T2_Bars > 0 condition
  
  analysisTable<-analysisTable[, MaxDistance_FromMA_Rounded:=floor(abs(MaxDistance_FromMA))]
  
  # Price At End
  PriceAtEnd <- list()
  PriceAtEnd<-groupTable[, last(Spread), by=get(whichGroup)]
  names(PriceAtEnd)[1]<- whichGroup
  
  analysisTable<-merge(analysisTable, PriceAtEnd, by=whichGroup)
  
  #Duration Max to End
  analysisTable[, DurationMaxToEnd:=abs(T1_T3_Bars-DurationTillMax)]
  
  setnames(analysisTable, c("DurationTillMax","V1", "DurationMaxToEnd"), c("T1_T2_Bars", "T3_price", "T2_T3_Bars"))
  # Calcualte Price Change
  analysisTable[, T2_T1_PriceCng:=round((T2_Price - T1_Price),4)][, T3_T2_PriceCng:=round((T3_price - T2_Price),4)][, T3_T1_PriceCng:=round((T3_price - T1_Price),4)]
  #Calculate ratio change between price and time
  analysisTable[, Speed:=round(T3_T1_PriceCng/T1_T3_Bars, 4)]
  #Calculate speed
  analysisTable[, SpeedTo_High:=round(T2_T1_PriceCng/T1_T2_Bars, 4)][, SpeedTo_End:=round(T3_T2_PriceCng/T2_T3_Bars, 4)][, SpeedTo_Start_End:=round(T3_T1_PriceCng/T1_T3_Bars, 4)]
  #filter small groups
  analysisTable<-analysisTable[get(whichGroup) > 0 & T1_T3_Bars > minBarforPatterns & T2_T3_Bars > 0 & T1_T2_Bars > 0 & abs(T3_T2_PriceCng) > 0 & abs(T2_T1_PriceCng) > as.numeric(min_T1_T2_Change)]
  analysisTable
}

CreateArrorChartsForSpeed <- function(dataTable){
  ggplot() + 
    geom_segment(data=dataTable, mapping=aes(x=0, y=0, xend=T1_T2_Bars+T2_T3_Bars, yend=T2_T1_PriceCng+T3_T2_PriceCng), arrow=arrow(), size=1, color=rownames(dataTable)) +
    geom_hline(aes(yintercept=0), colour="#990000", linetype="dashed") +
    ggtitle("Speed of each move Above and Below SMA") +
    
    labs(x="Time in Bars", y="Price change in Spread") +
    theme(plot.title = element_text(family = "Trebuchet MS", color="#666666", face="bold", size=20, hjust=0)) +
    theme(axis.title = element_text(family = "Trebuchet MS", color="#666666", face="bold", size=14)) 
}

CreateArrorChartsForRiseNRetrace <- function(dataTable){
  ggplot() + 
    geom_segment(data=dataTable, mapping=aes(x=0, y=0, xend=T1_T2_Bars, yend=T2_T1_PriceCng), arrow=arrow(), size=.6, color=rownames(dataTable)) + 
    geom_segment(data=dataTable, mapping=aes(x=T1_T2_Bars, y=T2_T1_PriceCng, xend=T1_T2_Bars+T2_T3_Bars, yend=T2_T1_PriceCng+T3_T2_PriceCng), arrow=arrow(), size=.6, color=rownames(dataTable)) + 
    geom_point(data=dataTable, mapping=aes(x=T1_T2_Bars, y=T2_T1_PriceCng), size=1, shape=21, fill="white")+
    geom_hline(aes(yintercept=0), colour="#990000", linetype="dashed") +
    ggtitle("Speed of making high and then Retracing back to SMA.") +
    labs(x="Time in Bars", y="Price change in Spread")+
    theme(plot.title = element_text(family = "Trebuchet MS", color="#666666", face="bold", size=20, hjust=0)) +
    theme(axis.title = element_text(family = "Trebuchet MS", color="#666666", face="bold", size=14)) 
}


Create4dCharts <- function(dataTable, whichGroup){
  #mainTable <- CeateMainGroupTable()
  #dataTable<-getFinalAnalysistablePerGroup(mainTable, whichGroup)  
  #print(dataTable)
  with(dataTable,
       scatter3D(T1_T2_Bars, T2_T1_PriceCng, T2_T3_Bars,bty = "g", pch = 20, cex = 2,col=T3_T2_PriceCng, theta = 60, phi = 20,
                 main = paste(whichGroup, "SMA T1 - T2 - T3 Analysis"), xlab = "Bars T1-T2",
                 ylab ="T2", zlab = "Bars T2-T3", ticktype = "detailed"))
}

#Create4dCharts("groupBelow")

#getFinalAnalysistablePerGroup(table, "groupBelow")

Create2dPriceComparisionCharts2 <- function(dataTable, whichGroup){
  #mainTable <- CeateMainGroupTable()
  #dataTable<-getFinalAnalysistablePerGroup(mainTable, whichGroup)  
  #print(dataTable)
  par(mfcol=c(1,4))
  xy1<-xyplot(T2_T1_PriceCng ~ T3_T2_PriceCng,  data = dataTable,  
         prepanel = function(x, y) prepanel.loess(x, y, span = 1),
         xlab = "T2_T1_Price Change", ylab = "T3_T2_Price Change", main="Price Comparision",
         panel = function(x, y) {
           panel.grid(h = -1, v = 2)
           panel.xyplot(x, y)
           panel.loess(x, y, span=1)
         },
         aspect = "xy")
  xy2<-xyplot(T1_T2_Bars ~ T2_T3_Bars,  data = dataTable,  
         prepanel = function(x, y) prepanel.loess(x, y, span = 1),
         xlab = "T1_T2_Bars", ylab = "T2_T3_Bars",main="Time Comparision",
         panel = function(x, y) {
           panel.grid(h = -1, v = 2)
           panel.xyplot(x, y)
           panel.loess(x, y, span=1)
         },
         aspect = "xy")
  xy3<-xyplot(T1_T2_Bars ~ T2_T1_PriceCng,  data = dataTable,  
         prepanel = function(x, y) prepanel.loess(x, y, span = 1),
         xlab = "T1_T2_Bars", ylab = "T2_T1_Price Change",main="T1-T2 Price/Time Comparision",
         panel = function(x, y) {
           panel.grid(h = -1, v = 2)
           panel.xyplot(x, y)
           panel.loess(x, y, span=1)
         },
         aspect = "xy")
  xy4<-xyplot( T2_T3_Bars ~ T3_T2_PriceCng,  data = dataTable,  
         prepanel = function(x, y) prepanel.loess(x, y, span = 1),
         xlab = "T2_T3_Bars", ylab = "T3_T2_Price Change",main="T2-T3 Price/Time Comparision",
         panel = function(x, y) {
           panel.grid(h = -1, v = 2)
           panel.xyplot(x, y)
           panel.loess(x, y, span=1)
         },
         aspect = "xy")
  #grid.arrange(xy1, xy2, xy3, xy4, ncol=4, nrow=1)
  print(xy1)#, pos = c(0.0, 0.0, 0.5, 0.5), more = TRUE, type='smooth')
  print(xy2)#, pos = c(0.0, 0.5, 0.5, 1.0), more = TRUE, type='smooth')
  print(xy3)#, pos = c(0.5, 0.0, 1.0, 0.5), more = TRUE, type='smooth')
  print(xy4)#, pos = c(0.5, 0.5, 1.0, 1.0), more = FALSE, type='smooth')
  par(mfcol=c(1,1))
}
#Create2dPriceComparisionCharts(dataTable, whichGroup)

Create2dPriceComparisionCharts <- function(dataTable, whichGroup){
  #mainTable <- CeateMainGroupTable()
  #dataTable<-getFinalAnalysistablePerGroup(mainTable, whichGroup)  
  #print(dataTable)
  PP <- ggplot(dataTable, aes(T2_T1_PriceCng, T3_T2_PriceCng)) + 
    geom_point() +  stat_smooth(method = "glm") 
   
  TT <- ggplot(dataTable, aes(T1_T2_Bars, T2_T3_Bars)) + 
    geom_point() +  stat_smooth(method = "glm") 
   
  PT12 <- ggplot(dataTable, aes(T1_T2_Bars, T2_T1_PriceCng)) + 
    geom_point() +  stat_smooth(method = "glm") 
  
  PT23 <- ggplot(dataTable, aes(T2_T3_Bars, T3_T2_PriceCng)) + 
    geom_point() +  stat_smooth(method = "glm")
  
  grid.arrange(PP, TT, PT12, PT23, ncol=4, nrow=1)
}
#Create2dPriceComparisionCharts2(dataTable, whichGroup)
#Create4dCharts("groupBelow")

Create2dPriceComparisionCharts1 <- function(dataTable, whichGroup){
  #mainTable <- CeateMainGroupTable()
  #dataTableA<-getFinalAnalysistablePerGroup(mainTable, "groupAbove", "durationAbove",2)  
  #dataTableB<-getFinalAnalysistablePerGroup(mainTable, "groupBelow", "durationBelow",2)  
  #rbind(dataTableA, dataTableB)
  #print(dataTable)
  #par(mfcol=c(2,2))
  dd<-melt(dataTable, id.var="groupAbove", measure.vars=c("T2_T1_PriceCng", "T3_T2_PriceCng"))
  temp1<-dataTable[order(-rank(T2_T3_Bars), T3_T2_PriceCng),]
  PTRatio<-dataTable[T3_T2_PriceCng !=0, c(T2_T1_PriceCng/T3_T2_PriceCng)]
  na.omit(PTRatio)
  da<-as.data.table(PTRatio)
  ggplot(temp1,aes(x=T2_T3_Bars, y=T3_T2_PriceCng, fill=T3_T2_PriceCng)) +
           geom_bar(stat="identity")
  ggplot(da) +    
    geom_bar(stat="identity") +
    geom_histogram(aes(PTRatio, ..density..), bins=10) +
    geom_density(aes(PTRatio, ..density..))+
    geom_rug(aes(PTRatio))
  
  
  temp2=temp1[, Ratio:=round(T2_T3_Bars/T1_T2_Bars, 2)][Ratio!=0, .(groupAbove, T2_T3_Bars,T1_T2_Bars, Ratio)]
  temp1.m<-melt(temp1[, .(T2_T3_Bars, T3_T2_PriceCng)])
  
  ggplot(temp2) +
    geom_histogram(aes(x = Ratio, y = ..density..), bins=10, fill="green")+
    geom_density(aes(Ratio, ..density..))+
    geom_rug(aes(Ratio))
  
  
  ggplot(foo, aes(x=day, y=c(var), fill=category)) +
    geom_bar(stat="identity")
  
  ggplot(dataTable, aes(x=T2_T1_PriceCng, y=T3_T2_PriceCng))+geom_bar()
  
  #print(xy1, pos = c(0.0, 0.0, 0.5, 0.5), more = TRUE, type='smooth')
  #print(xy2, pos = c(0.0, 0.5, 0.5, 1.0), more = TRUE, type='smooth')
  #print(xy3, pos = c(0.5, 0.0, 1.0, 0.5), more = TRUE, type='smooth')
  #print(xy4, pos = c(0.5, 0.5, 1.0, 1.0), more = FALSE, type='smooth')
  par(mfcol=c(1,1))
}

CreateDensityCharts <- function(dataTableAbove,dataTableBelow, mtitle, xlab, intercept, TimeOrPrice, startFrom){
  #mainTable <- CeateMainGroupTable()
  #dataTable<-getFinalAnalysistablePerGroup(mainTable, whichGroup)  
  #print(dataTable)
  aboveDT.m <-NULL
  belowDT.m <-NULL
  if(TimeOrPrice=="T"){
    aboveDT.m <- melt(dataTableAbove[T2_T1_PriceCng >= startFrom, .(TimeRatio_AboveSMA)])
    belowDT.m <- melt(dataTableBelow[T2_T1_PriceCng >= startFrom, .(TimeRatio_BelowSMA)])
  } else if (TimeOrPrice=="P"){
    aboveDT.m <- melt(dataTableAbove[T2_T1_PriceCng >= startFrom, .(PriceRatio_AboveSMA)])
    belowDT.m <- melt(dataTableBelow[T2_T1_PriceCng >= startFrom, .(PriceRatio_BelowSMA)])
  }
  l<-list(aboveDT.m, belowDT.m)
  mergedAboveBelow<-rbindlist(l, use.names=T, fill=T)

  g<-ggplot(mergedAboveBelow) +
   # geom_histogram(aes(x = value, y = ..density.., color=variable), bins=10)+
    geom_density(aes(value, ..density.., fill=variable),alpha=.3)+
    geom_rug(aes(value)) +
    ggtitle(mtitle) +
    labs(x=xlab, y='Density') +
    geom_vline(xintercept=intercept, linetype=1, size=.8)+ ## 0 = blank, 1 = solid, 2 = dashed, 3 = dotted, 4 = dotdash, 5 = longdash, 6 = twodash
    theme(plot.title = element_text(family = "Trebuchet MS", color="#666666", face="bold", size=20, hjust=0)) +
    theme(axis.title = element_text(family = "Trebuchet MS", color="#666666", face="bold", size=14)) +
    scale_fill_manual("Groups",values=c("orange","red"))
  g
}

calculateProbabilityofCycles <- function(dataTableAbove,dataTableBelow, column1, referencePoint, showAboveFilter, title, xlab,legend1, legend2){
  print('enter calculateProbability')
  
  AboveDT <- dataTableAbove[T2_T1_PriceCng > showAboveFilter, get(column1)]
  BelowDT <- dataTableBelow[T2_T1_PriceCng > showAboveFilter, get(column1)]
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
  # create the plot directly with just the plot.ecdf() function, but this doesn't produce any empirical CDF values
  plot.ecdf(AboveDT,xlab = xlab, ylab = '',bg="black", main = title, col=aHistColor,do.points = F, lwd =1)
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





