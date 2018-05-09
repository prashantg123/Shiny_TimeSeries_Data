getFullPointValue<-function(inst){
  pointValue<-1
  if(inst=="ES")
    pointValue=50
  else if(inst=="SPI")
    pointValue=25 #* .74
  else if(inst=="DX") #DAX
    pointValue=25 #* 1.1
  else if(inst=="DW") #DOW
    pointValue=5
  else if(inst=="STX") #STOXX
    pointValue=10 #* 1.1
  else if(inst=="TSX") #FTSE Pound
    pointValue=10 #* 1.4 
  else if(inst=="NKI") #Nikki
    pointValue=500 #* .0086
  return(pointValue)
}
# DAX"="DX", "DOW"="DW", "STOXX"="STX", "FTSE"="TSX", "NKY"="NKI", "SPI"="SPI", "OIL"="CL"

getExchangeRateinUSD<-function(curr){
  switch(inst,
         AUD=.7,
         EUR=1.1
  )
}
 as.numeric(getFullPointValue("SPlI"))
