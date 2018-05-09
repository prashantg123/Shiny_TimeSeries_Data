#clear workspace (for testing)
rm(list = ls())

#load libraries
require(quantmod)
require(PerformanceAnalytics)
require(blotter)
require(FinancialInstrument)
require(quantstrat)
require(forecast)
require(foreach)

#clear portfolio and acct not needed due to the clearing workspace but here incase you don't use it.
suppressWarnings(rm("account.stocky","portfolio.stocky",pos=.blotter))
suppressWarnings(rm("order_book.stocky",pos=.strategy))
suppressWarnings(rm(stocky))

#if your stock is different you need to change (initdate,initportf,addposlimit, chart.posn)
symbol = "SPY"

#Set up currencies
currency("USD")

#define stock change spy to your stock of choice
stock(symbol, currency="USD", multiplier = 1)

####################################################### Get Data #################################################
getSymbols(symbol,src="yahoo",from = "2000-01-01" )

#Set Initial Date and Equity, note change SPY to your stock of choice.
initDate = start(SPY)
initEq = 10000

########################## Set up portfolio orders and Acct #######################################
#change SPY to your stock choice

initPortf(name="stocky","SPY",initPosQty=0,initDate=initDate,currency="USD")
initAcct("stocky",portfolios="stocky",initDate=initDate,initEq=initEq)
initOrders("stocky",symbols=symbol,initDate=initDate)

#position limits
#change "SPY" to your stock choice
addPosLimit("stocky","SPY",timestamp=initDate,maxpos=100, minpos=0)

#Set up Strategy
stratstocky<-strategy("stocky")

##############################FUNCTIONS#################################

autoregressor1  = function(x){
  if(NROW(x)<32){ result = NA} else{
    y = ROC(Ad(x))
    y = na.omit(y)
    step1 = ar.yw(y)
    step2 = predict(step1,newdata=y,n.ahead=1)
    step3 = step2$pred[1]+1
    step4 = (step3*last(Ad(x))) - last(Ad(x))
    
    result = step4
  }
  return(result)
}

autoregressor = function(x){
  ans = rollapply(x,50,FUN = autoregressor1,by.column=FALSE)
  return (ans)}

########################indicators#############################

stratstocky<-add.indicator(
  strategy  =	stratstocky, 
  name		=	"autoregressor", 
  arguments	=	list(
    x		=	quote(mktdata)),
  label		=	"arspread")

################################################ Signals #############################

stratstocky<-add.signal(
  strategy			= stratstocky,
  name				= "sigThreshold",
  arguments			= list(
    threshold		= 0.25,
    column			= "arspread",
    relationship	= "gte",
    cross			= TRUE),
  label				= "Selltime")

stratstocky<-add.signal(
  strategy			= stratstocky,
  name				= "sigThreshold",
  arguments			= list(
    threshold		= 0.1,
    column			= "arspread",
    relationship	= "lt",
    cross			= TRUE),
  label				= "cashtime")

stratstocky<-add.signal(
  strategy  		= stratstocky,
  name				= "sigThreshold",
  arguments			= list(
    threshold		= -0.1,
    column			= "arspread",
    relationship	= "gt",
    cross			= TRUE),
  label				= "cashtime")

stratstocky<-add.signal(
  strategy  		= stratstocky,
  name				= "sigThreshold",
  arguments			= list(
    threshold		= -0.25,
    column			= "arspread",
    relationship	= "lte",
    cross			= TRUE),
  label				= "Buytime")

######################################## Rules #################################################

#Entry Rule Long
stratstocky<- add.rule(stratstocky,
                       name				=	"ruleSignal",
                       arguments			=	list(
                         sigcol			=	"Buytime",
                         sigval			=	TRUE,
                         orderqty		=	100,
                         ordertype		=	"market",
                         orderside		=	"long",
                         pricemethod		=	"market",
                         replace			=	TRUE,
                         TxnFees				=	-1,
                         osFUN				=	osMaxPos), 
                       type				=	"enter",
                       path.dep			=	TRUE,
                       label				=	"Entry")

#Entry Rule Short

stratstocky<- add.rule(stratstocky,
                       name  			=	"ruleSignal",
                       arguments			=	list(
                         sigcol			=	"Selltime",
                         sigval			=	TRUE,
                         orderqty		=	100,
                         ordertype		=	"market",
                         orderside		=	"short",
                         pricemethod		=	"market",
                         replace			=	TRUE,
                         TxnFees				=	-1,
                         osFUN				=	osMaxPos), 
                       type				=	"enter",
                       path.dep			=	TRUE,
                       label				=	"Entry")

#Exit Rules

#Exit 
stratstocky <- add.rule(stratstocky,
                        name				=	"ruleSignal",
                        arguments			=	list(
                          sigcol				=	"cashtime", 
                          sigval			=	TRUE, 
                          orderqty		=	"all", 
                          ordertype		=	"market",
                          orderside		=	"long", 
                          pricemethod		=	"market",
                          replace			=	TRUE,
                          TxnFees			=	-1),
                        type			=	"exit",
                        path.dep			=	TRUE,
                        label				=	"Exit")

##############################    Apply Strategy ##############################################

out <- applyStrategy(strategy=stratstocky, portfolios="stocky")
updatePortf("stocky")

############################# Portfolio Return Characterics ################################
#get portfolio data
portRet <- PortfReturns("stocky")
portRet$Total <- rowSums(portRet, na.rm=TRUE)
charts.PerformanceSummary(portRet$Total)
#tradeStats("stocky")[,c("Symbol","Num.Trades","Net.Trading.PL","maxDrawdown
#change SPY to your stock choice
chart.Posn("stocky","SPY")
results1<-getTxns("stocky","SPY")
#plot(results1$Net.Txn.Realized.PL)