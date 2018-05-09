### Initial setup
rm(strategy.st)
try(rm("account.st","portfolio.st"),silent=TRUE)  

.blotter <- new.env()
.strategy <- new.env()

initDate <- as.character(as.Date(from) - 1)
currency("USD")
Sys.setenv(TZ = "UTC")   
symbols <- "data"
stock(symbols, currency = "USD", multiplier = 1)  # Initialisation of the instrument
tradeSize <- 1                                    # Initialisation of trade size
initEq <- 1000                                    # Initialisation of initial equity

strategy.st <- "btc"                              # Initialisation of the strategy
portfolio.st <- "btc"                             # Initialisation of the strategy, must be after strategy
account.st <- "btc"                               # Initialisation of the strategy, must be after strategy and portolio


initPortf(portfolio.st, symbols=symbols, initDate=initDate, currency='USD')
initAcct(account.st, portfolios=portfolio.st, initDate=initDate, currency='USD',initEq=initEq)
initOrders(portfolio.st, initDate=initDate)
strategy(strategy.st, store=TRUE)  

### Parametres
lookBackVol <- 5
thresholdVol <- 20
stopLoss <- -0.01
profitTarget <- 0.01

### Indicators
add.indicator(strategy.st, name = "runSum", arguments = list(x = quote(Vo(AAPL)), n = lookBackVol), label = "volRunSum")

### Signals
add.signal(strategy.st, name = "sigThreshold", arguments = list(column = "volRunSum", threshold = thresholdVol, relationship = "gte", cross = TRUE), label = "longSig")

### Rules
add.rule(strategy = strategy.st, name = "ruleSignal",
         arguments = list(sigcol = "longSig", sigval = 1,
                          orderqty = tradeSize,
                          ordertype = "market",
                          orderside = "long",
                          prefer = "Close",
                          replace = FALSE
         ),
         type = "enter",
         label = "enterLong",
         path.dep = TRUE
)


add.rule(strategy.st, name = "ruleSignal",
         arguments = list(sigcol = "longSig", sigval = 1,
                          orderqty = "all",
                          ordertype = "limit",
                          orderside = "long",
                          prefer = "Close",
                          replace = FALSE,
                          tmult = TRUE,
                          threshold = quote(profitTarget),
                          orderset = "ocolong"
         ),
         type = "chain",
         parent = "enterLong",
         label = "profitTargetLong",
         path.dep = TRUE
)


add.rule(portfolio.st, name = "ruleSignal",
         arguments = list(sigcol = "longSig", sigval = 1,
                          orderqty = "all",
                          ordertype = "stoplimit",
                          orderside = "long",
                          prefer = "Close",
                          replace = FALSE,
                          tmult = TRUE,
                          threshold = quote(stopLoss),
                          orderset = "ocolong"
         ),
         type = "chain",
         parent = "enterLong",
         label = "stopLossLong",
         path.dep = TRUE
)


test <- applyIndicators(strategy.st, mktdata)
test <- applySignals(strategy.st, applyIndicators(strategy.st, quote(Vo(mktdata))))
head(test, 20)

### Results
results <- applyStrategy(strategy.st, portfolio.st)
