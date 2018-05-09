library (data.table)
library(lubridate)
rollData <- fread("D:/QuantMagnum/TWAllData_Slicer/ContractRolls2.csv")
names(rollData)<- c("StartDate", "RollContract", "EndDate")

#files <- list.files(path = "D:/QuantMagnum/TWAllData_Slicer/Data",pattern = ".csv")

allContracts<-dir(path = "D:/QuantMagnum/TWAllData_Slicer/Data/All/", pattern = ".csv", all.files = FALSE,
    full.names = F, recursive = FALSE,
    ignore.case = FALSE, include.dirs = FALSE, no.. = FALSE)

allContracts<-substr(AllDataFiles, 1,5)

for(i in seq(1,length(allContracts),1))
{
  eachContracts= allContracts[i]
  print(paste("-----------------------", "Starting Contract", eachContracts, " -------------------------------------------------------"))
  print(eachContracts)
  startDate <- rollData[RollContract==eachContracts,StartDate]
  endDate <- rollData[RollContract==eachContracts,EndDate]
  if(length(startDate) >0 ){
    trucateStartDate <- dmy_hm(startDate)-days(2)
    trucateEndDate <- dmy_hm(endDate)+days(2)
    rawAllData <- fread(paste0("D:/QuantMagnum/TWAllData_Slicer/Data/All/",eachContracts,".csv"))
    print("RawData")
    print(rawAllData)
    setnames(rawAllData, "Date and Time", "Date_and_Time")
    print("trucateStartDate")
    print(trucateStartDate)
    print("trucateEndDate")
    print(trucateEndDate)
    
    truncatedData<-rawAllData[dmy_hms(Date_and_Time) > ymd_hms(trucateStartDate) & dmy_hms(Date_and_Time) < ymd_hms(trucateEndDate),]
    if(nrow(truncatedData)>10){
      print(truncatedData)
      print("Writing to file")
       write.csv(truncatedData, paste0("D:/QuantMagnum/TWAllData_Slicer/Data/All/Trunc/",eachContracts,".csv"))
    } else {
      print(paste("No data truncated for contract", eachContracts))
    }
  } else {
    print(paste("Contract", eachContracts, "not found."))
          print("")
  }
  print(paste("--------------------- Finish Contract ", eachContracts, " ----------------------"))
}


