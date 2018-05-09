
library(lubridate)
library(data.table)
library(RMySQL)
library(quantmod)
library(PerformanceAnalytics)

data<-fread("D:/SHINYPROJECT/StrategyAnalysis/FinalPortfolioSeries.csv")
data
fixedCurrency=F
usePrice="PCT"

if(fixedCurrency==TRUE){
  fixedCurrencyValue <- 75.76
} else {
  fixedCurrencyValue <- data$xtsData3.Close
}
if(usePrice=="P"){
  AUD_P<-data[,xtsData3.Close]
  EM1_P<-data[,xtsData1.Close]
  SPI_P<-data[,xtsData2.Close  * fixedCurrencyValue/100]
} else if(usePrice=="D"){
  AUD_D<-data[,xtsData3.Close * 1000]
  EM1_D<-data[,xtsData1.Close * 50]
  SPIAUD_D<-data[,xtsData2.Close * (fixedCurrencyValue/100)  * 25]
#} else if(usePrice=="PCT"){
  AUD_PC<-diff(log(AUD_D))
  EM1_PC<-diff(log(EM1_D))
  SPIAUD_PC<-diff(log(SPIAUD_D))
}

#=============================================================EM1 ~  SPIAUD + AUD)

reg<- lm(EM1_P ~  SPI_P + AUD_P)
reg
NewSpread<- -EM1_P + coef(reg)["SPI_P"] * SPI_P +  coef(reg)["AUD_P"] * AUD_P
OriSpread<-  -EM1_P + SPI_P -  AUD_P
sm<-summary(reg)
#sm
slope<-coef(lm((1:length(NewSpread)) ~ NewSpread))["NewSpread"]
text<-paste0("PRICE - (-1*EM " , round(coef(reg)["SPI_P"],4),"*SPI ", round(coef(reg)["AUD_P"],4), "*AUD", ") |MSE = ", round(mean(sm$residuals ^ 2), 2),  "Slope ", round(slope, 2))
print(text)
#plot(OriSpread, type='l');title(paste("Original ", text))
plot(NewSpread, type='l');title(paste("SD ", round(sd(NewSpread),2), " ", text))
#----------------------------------------------------------------PCT

reg<- lm(EM1_PC ~  SPIAUD_PC + AUD_PC)
reg
sm<-summary(reg)
#sm
NewSpread<- -EM1_PC + coef(reg)["SPIAUD_PC"] * SPIAUD_PC +  coef(reg)["AUD_PC"] * AUD_PC
OriSpread<-  -EM1_PC + SPIAUD_PC -  AUD_PC
slope<-coef(lm((1:length(NewSpread)) ~ NewSpread))["NewSpread"]
text<-paste0("PCT - (-1*EM " , round(coef(reg)["SPIAUD_PC"],4),"*SPIAUD ", round(coef(reg)["AUD_PC"],4), "*AUD", ") | MSE = ", round(mean(sm$residuals ^ 2), 4), "Slope ", round(slope, 2))
print(text)
#plot(OriSpread, type='l');title(paste("Original ", text))
plot(NewSpread, type='l');title(paste("SD ", round(sd(NewSpread),4), " ", text))



#-----------------------------------------------------------------------Dollar

reg<- lm(EM1_D ~  SPIAUD_D + AUD_D)
reg
sm<-summary(reg)
#sm
NewSpread<- -EM1_D + coef(reg)["SPIAUD_D"] * SPIAUD_D +  coef(reg)["AUD_D"] * AUD_D
OriSpread<-  -EM1_D + SPIAUD_D -  AUD_D
slope<-coef(lm((1:length(NewSpread)) ~ NewSpread))["NewSpread"]
text<-paste0("DOLLAR(-1*EM " , round(coef(reg)["SPIAUD_D"],4),"*SPIAUD ", round(coef(reg)["AUD_D"],4), "*AUD", ") | MSE = ", round(mean(sm$residuals ^ 2), 2), "Slope ", round(slope, 2))
print(text)

#plot(OriSpread, type='l');title(paste("Original ", text))
plot(NewSpread, type='l');title(paste("SD", round(sd(NewSpread),4), text))








reg<- lm(AUD_P ~ EM1_P + SPI_P)
reg
OriSpread<- -AUD_P - EM1_P + SPI_P
NewSpread<- -AUD_P + coef(reg)["EM1_P"] * EM1_P + coef(reg)["SPI_P"] * SPI_P
sm<-summary(reg)
#sm
slope<-coef(lm((1:length(NewSpread)) ~ NewSpread))["NewSpread"]
text<-paste0("PRICE - (-1*AUD " , round(coef(reg)["EM1_P"],4),"*EM1 ", round(coef(reg)["SPI_P"],4), "*SPIAUD", ") | MSE = ", round(mean(sm$residuals ^ 2), 2), round(slope, 2))
print(text)
#plot(OriSpread, type='l');title(paste("Original ", text))
plot(NewSpread, type='l');title(paste("SD ", round(sd(NewSpread),2), " ", text))

#-------------------------------------------------------


reg<- lm(AUD_D ~ EM1_D + SPIAUD_D)
reg
sm<-summary(reg)
#sm
OriSpread<- -AUD_D - EM1_D + SPIAUD_D
NewSpread<- -AUD_D + coef(reg)["EM1_D"] * EM1 + coef(reg)["SPIAUD_D"] * SPIAUD_D
slope<-coef(lm((1:length(NewSpread)) ~ NewSpread))["NewSpread"]
text<-paste0("DOLLAR - (-1*AUD " , round(coef(reg)["EM1_D"],4),"*EM1 ", round(coef(reg)["SPIAUD_D"],4), "*SPIAUD", ") | MSE = ", round(mean(sm$residuals ^ 2), 2), round(slope, 2))
print(text)

#plot(OriSpread, type='l');title(paste("Original ", text))
plot(NewSpread, type='l');title(paste("SD ", round(sd(NewSpread),2), " ", text))





























#=============================================================SPIAUD ~  EM1 + AUD)


reg<- lm(SPI_P ~  EM1_P + AUD_P)
reg
NewSpread<- -SPI_P + coef(reg)["EM1_P"] * EM1_P +  coef(reg)["AUD_P"] * AUD_P
OriSpread<-  -SPI_P + SPI_P -  AUD_P
sm<-summary(reg)
#sm

text<-paste0("PRICE - (-1*SPIAUD " , round(coef(reg)["EM1_P"],4),"*EM ", round(coef(reg)["AUD_P"],4), "*AUD", ") | MSE = ", round(mean(sm$residuals ^ 2), 2))
print(text)
#plot(OriSpread, type='l');title(paste("Original ", text))
#plot(NewSpread, type='l');title(paste("SD ", sd(NewSpread), " ", text))

#=============================================================

reg<- lm(SPIAUD_D ~  EM1_D + AUD_D)
reg
sm<-summary(reg)
#sm
slope<-coef(lm((1:length(NewSpread)) ~ NewSpread))["NewSpread"]
text<-paste0("DOLLAR - (-1*SPIAUD " , round(coef(reg)["EM1_D"],4),"*EM ", round(coef(reg)["AUD_D"],4), "*AUD", ") | SLOPE = ", round(slope, 2))
print(text)
OriSpread<-  -SPIAUD_D + EM1_D -  AUD_D
NewSpread<- -SPIAUD_D + coef(reg)["EM1_D"]  * EM1_D +  coef(reg)["AUD_D"]  * AUD_D

#plot(OriSpread, type='l');title(paste("Original ", text))
plot(NewSpread, type='l');title(paste("SD ", sd(NewSpread), " ", text))

