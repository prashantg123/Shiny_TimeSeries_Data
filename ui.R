library(dygraphs)

instrumetnsList<-c(Choose='', "AUD"="AD", "EUR"="EC", "CAD"="CD", "YEN"="JY", "GBP"="BP", "BUND"="BN", "BOBL"="EU_BB", "BUXL"="EU_BX", "SHATZ"="EU_2SH",  "CB"="CB", "OAT"="OAT", "BTP"="BTP",
                   "GILT"="GL", "JGB"="JB", "ED"= "ED5", "TU"="TU", "FV"="FV", "TY"="TY", "US"="US", "US_ULTRA"="US_ULTRA","XT"="AX", "YT"="AY", "XX"="AU_XX", "IR1"="AU1", "IR2"="AU2", "IR3"="AU3", "IR4"="AU4", "IR5"="AU5",
                   "EMINI"="ES", "DAX"="DX", "DOW"="DW", "STOXX"="STX", "FTSE"="ftse", "NKY"="NKI", "SPI"="SPI", "TOPIX"="TOP", "TAIWAN"="TAIW", "DINX"="DINX",
                   "OIL"="CL", "GAS"="ng", "CORN"="CR", "WHEAT"="WH", "SOYBEAN"="SOY", "SOY_MEAL"="SOYM", "SOY_OIL"="SOYO", "COFFEE"="COF", "COPPER"="CP", "GOLD"="GC", "SILVER"="SI")
currencyList<-c(Choose='', "USD"="UD", "AUD"="AD", "EUR"="EU", "CAD"="CD", "YEN"="JY")



shinyUI(fluidPage(
  tags$head(
    tags$style(HTML("
      .shiny-output-error-validation {
        color: green;
      }
    "))
  ),
  #headerPanel("Hello Shiny!"),
  titlePanel(div("Modelling & Analysis Tool"), windowTitle="MA Distance"),#style = "height:50px;background-color: blue;", "Topleft",
  #img(src="win.jpg",height=100,width=500,align="center"),
  
  #title = "Diamonds Explorer",
  hr(),
  
  sidebarLayout(
    # Sidebar with a slider input
    tags$head(
    sidebarPanel(
      HTML('<script type="text/javascript">
        $(document).ready(function() {
          $("#DownloadButton").click(function() {
           $("#Download").text("Loading...");
           });
           });
           </script>
           '),
        tags$link(rel="stylesheet", type="text/css", href="StyleFile.css"),
        strong("Select Date Range:"),
        fluidRow(
          
          dateRangeInput("Date", "", start = "2016-01-01", end = "2017-09-30" , min = Sys.Date()-4000,
                         max = Sys.Date()+1000, format = "yyyy-mm-dd", startview = "year", weekstart = 1,
                         language = "en", separator = " to ", width = '250px')
#           hr()
          ),
        strong("Select Database:"),
        fixedRow(
          
          column(7, radioButtons("rdoDatabase", "",list("Original Price", "Rolled Price"), selected = "Rolled Price", inline=T)
          )
        ),
#           fluidRow(
#         hr(),
        strong("Create Model/Structure/Portfolio:"),
        fluidRow(
          column(3, selectInput('Inst1',"" , instrumetnsList,selected="AD", selectize=TRUE, width = '120px')
          ),
          column(4, textInput("Ratio1","",588, width = '120px')
          ),
          column(5, selectInput('Inst6', "" , currencyList,selected="USD", selectize=TRUE, width = '100px')
          )
        ),
        fluidRow(
          column(3, selectInput('Inst2', NULL, instrumetnsList, selected="TY",selectize=TRUE, width = '120px')
          ),
          column(4, textInput("Ratio2",NULL ,-12, width = '120px')
          ),
          column(5, selectInput('Inst7', NULL, currencyList,selected="USD", selectize=TRUE, width = '100px')
          )
        ),
        fluidRow(
          column(3, selectInput('Inst3', NULL, instrumetnsList, selected="AX", selectize=TRUE, width = '120px')
          ),
          column(4, textInput("Ratio3",NULL, 100, width = '120px')
          ),
          column(5, selectInput('Inst8', NULL, currencyList,selected="USD", selectize=TRUE, width = '100px')
          )
        ),
        fluidRow(
          column(3, selectInput('Inst4', NULL, instrumetnsList, selected="", selectize=TRUE, width = '120px')
          ),
          column(4, textInput("Ratio4",NULL,1, width = '120px')
          ),
          column(5, selectInput('Inst9', NULL , currencyList,selected="USD", selectize=TRUE, width = '100px')
          )
        ),
        fluidRow(
          column(3, selectInput('Inst5', NULL, instrumetnsList, selectize=TRUE, width = '120px')
          ),
          column(4, textInput("Ratio5",NULL,1, width = '120px')
          ),
          column(5, selectInput('Inst10', NULL, currencyList,selected="USD", selectize=TRUE, width = '100px')
          )
        ),
        strong("Model Type:"),
        fluidRow(
#           helpText("Click to choose Bond Model or Index Model."),
          column(5, radioButtons("rdoIndexOrBond", "",list("Bond Model", "Index Model"), selected = "Bond Model", inline=T)
          #column(4, checkboxInput('chkIndexOrBond', "Bond Model", value=TRUE, width = '150px'), h6("(Uncheck for index Model)")
          )
        ),
#         ),
        strong("Select Time interval:"),
        fluidRow(
          column(3, numericInput("nInterval",'',60, width = '150px')
          ),
          column(4, selectInput('Interval', '', c("mins", "hours", "daily", "weekly"), selectize=TRUE, width = '150px')
          )
        ),
        fluidRow(
          
          column(3, numericInput("Normalize",'Normalize By',1, width = '100px')),
          column(3, numericInput("BinsBy","Bins By", 15, width = '100px'))
        ),
        strong("Spread decimal multiplier: "),
        fluidRow(
          column(3, numericInput("nMultiplier",'',1, width = '150px')
          )
        ),
        fluidRow(
          column(3, numericInput("nMinBP",'Min BP:',1, width = '150px')
          ),
          column(3, numericInput("nMaxBP",'Max BP',12, width = '150px')
          )
        ),
        strong("Include patterns greater than (BP):"),
        fluidRow(
          column(3, textInput("nFilter",NULL, 0, width = '120px')
          )
        ),
        fluidRow(
          hr(),
          column(5, actionButton("goButton", "Test Structure"), offset = 2 #submitButton("Create Charts")
          )
        )
      )#head
    ),#sidebarPanel
    mainPanel(
      fixedRow(
        #column(1,strong("SMA: ")),
        column(3,numericInput("SMA","SMA", 200, width = '100px')),
        column(4,textInput("RefValue","Ref Value", 4, width = '150px')),
        column(5,textInput("ExtreamValue","Extream Value", 4, width = '170px'))
        
        #column(3, actionButton("goButton", "Create Charts") #submitButton("Create Charts")
        #),
        #div(strong("SMA: "), numericInput("SMA","", 200, width = '150px'),width = '350px')
        #div(strong("To: "), textOutput("to", inline = TRUE))
        #column(2, textOutput("VolOut")
        #)
        ),
      fluidRow(
       # hr(),
       
        tabsetPanel(id= "tabPanels", type = "tabs", 
                    tabPanel("Price Chart", 
                             fluidRow(
                                     column(12, dygraphOutput("PricePlot"))
                             ),
                             hr(),
                             tags$h4(" First and last row of dataset."),
#                              hr(),
                             fluidRow(
                                     column(12, dataTableOutput("priceTable"),
                                            tags$head(tags$style("#dummy table {background-color: red; }", media="screen", type="text/css"))
                                     )#tableOutput('table'))
                             ),
                             value="panel1" ), 
                    tabPanel("Value Prob",
                             tags$h4("Probability calculated from vertical distance from MA."),
                             fluidRow(
                               column(12, plotOutput("Value_Price_ProbChart"))
                             ),
                             hr(),
                             tags$h4(" All Values > Extream Value."),
                             fluidRow(
                                column(12, dataTableOutput("ExtreamValueTable_Value"))#tableOutput('table'))
                             ),
                             value="panel2"),
                    tabPanel("Risk Prob", 
                             tags$h4("Probability calculated by keeping reference point fixed (MA-Price cross)."),
                             hr(),
                             fixedRow(
                               column(12, div( plotOutput("Risk_Price_ProbChart")))
                             ),
                             hr(),
                             fixedRow(
                               column(12, div( plotOutput("Risk_Time_ProbChart")))
                             ),
                             hr(),
                             tags$h4(" All Values."),
                             fixedRow(
                               column(12, dataTableOutput("ExtreamValueTable_Risk"))#tableOutput('table'))
                             ),
                             value="panel2" ), 
                    tabPanel("Value Bins",
                              tags$h4("Number of Trades falls in each range."),
                              hr(),
                              fluidRow(
                                
                                column(3, numericInput("Normalize",'Normalize By',1, width = '100px')),
                                column(3, numericInput("BinsBy","Bins By", 10, width = '100px'))
                              ),

                             fluidRow(
                               #column(12, div(tableOutput("BinsTable")))
                               column(12, div(dataTableOutput("BinsTable_Value")))
                             ),
                              hr(),
                             value="panel3" ), 
                    tabPanel("Risk Bins",
                             tags$h4("Number of Trades falls in each range."),
                             hr(),
                             
                             fluidRow(
                               #column(12, div(tableOutput("BinsTable")))
                               column(12, div(dataTableOutput("BinsTable_Risk")))
                             ),
                             hr(),
                             tags$h4("PRICE probabilities in 2 parts from start to H/L and then Retatrace to SMA."),
                             hr(),
                             fluidRow(
                               column(12, div(plotOutput("PriceTime3_1")))
                             ),
                             tags$h4("PRICE probabilities in 2 parts from start to H/L and then Retatrace to SMA."),
                             hr(),
                             fluidRow(
                               column(12, div(plotOutput("PriceTime3_2")))
                             ),
                             tags$h4("Price-Time 2D results for Above MA"),
                             fixedRow(
                               column(12, plotOutput("PriceTime3_2D_1"))
                             ),
                             tags$h4("Price-Time 2D results for Below MA"),
                             fixedRow(
                               column(12, plotOutput("PriceTime3_2D_2"))
                             ),
                             value="panel3" ), 
                    tabPanel("Patterns", 
                             hr(),
                             fluidRow(
                               column(12, div(plotOutput("Arrow_Extream")))
                             ),
                             hr(),
                             fluidRow(
                               column(12, div(plotOutput("Arrow_StartToEnd")))
                             ),
                             # hr(),
                             # fluidRow(
                             #   column(12, div(plotOutput("Value_Bars")))
                             # ),
                             hr(),
                             tags$h4(" All Values."),
                             fixedRow(
                               column(12, dataTableOutput("ExtreamValueTable_Pricetime_2"))#tableOutput('table'))
                             ),
                             value="panel4" ), 
                    #tabPanel("Probability Chart", plotOutput("ProbPlot"), value="panel2"),
                    tabPanel("Stats",  
                             #plotOutput("BoxPlot_Value"), 
                             br(),
                             tags$h4("Box plot of Value (Vertical distance from MA)"),
                             plotOutput("BoxPlot_Value"),
                             br(), 
                             h4("Above"),
                             tableOutput("SummaryAbove_Value"), 
                             h4("Below"), 
                             tableOutput("SummaryBelow_Value"),

                             hr(),
                             br(),
                             tags$h4("Box Plot of Risk (Max distance from Price cross MA)"),
                             plotOutput("BoxPlot_Risk"),
                             br(), 
                             h4("Above"),
                             tableOutput("SummaryAbove_Risk"), 
                             h4("Below"), 
                             tableOutput("SummaryBelow_Risk"),
                             br(),
                             value="panel3"),
                    tabPanel("Partition",  
                             #plotOutput("BoxPlot_Value"), 
                             tags$h4("Partition time sereis in different regular periods."),
                             hr(),
                             fluidRow(
                               column(2, dateInput("ReferenceDate",'Reference Date', value= Sys.Date()-250, min = Sys.Date()-4000,
                                                    format = "yyyy-mm-dd", startview = "year", weekstart = 1, width = '120px')),
                               column(2, numericInput("DaysBefore",'Days Before',15, width = '100px')),
                               column(2, numericInput("DaysAfter",'Days After',15, width = '100px')),
                               column(2, textInput("ByEvery","By Every", '-3 month', width = '100px')),
                               column(2, numericInput("LookBackPeriod","LookBack Period", 6, width = '130px')),
                               column(5, actionButton("partitionButton", "Partition it"), offset = 0) #submitButton("Create Charts")
                             ),
                             
                             hr(),
                             tags$h4("Line chart showing days before and after the selected date for each period."),
                             plotOutput("BeforeSegmentPlot"),
                             plotOutput("AfterSegmentPlot"),
                             br(),
                             tags$h4("Stats for each period in basis points."),
                             hr(),
                             h5("Before and After reference date:"),
                             tableOutput("SegmentResults"), 
                             h5("Average stats for Before and After:"), 
                             tableOutput("SegmentAverageResults"),
                             h5("Cross Compare probabilities of After and Before reference date:"), 
                             tableOutput("SegmentCrossComapare"),
                             hr(),
                             br(),
                             value="panel5"),
                  tabPanel("Event",  
                           #plotOutput("BoxPlot_Value"), 
                           tags$h4("Events and Economic Data based analysis."),
                           hr(),
                           fluidRow(
                             column(2, dateInput("EventStartDate",'Event StartDate', value= Sys.Date()-1000, min = Sys.Date()-4000,
                                                 max = Sys.Date(), format = "yyyy-mm-dd", startview = "year", weekstart = 1, width = '120px')),
                             column(2, numericInput("DaysBeforeEvent",'Days Before',5, width = '100px')),
                             column(2, numericInput("DaysAfterEvent",'Days After',5, width = '100px')),
                             column(4, selectInput('EventName', '', c("AUSRateCut", "NFP"), selectize=TRUE, width = '150px')),
                             column(5, actionButton("eventNewsButton", "Show!"), offset = 0) #submitButton("Create Charts")
                           ),
                           hr(),
                           tags$h4("Line chart showing days before and after the selected date for each period."),
                           plotOutput("EventPlot"),
                           hr(),
                           br(),
                           value="panel6"),
                    # tabPanel("Result Confi", 
                    #          tags$h4("Probability calculated by keeping reference point fixed (MA-Price cross)."),
                    #          hr(),
                    #          tags$h4("Charts showing comparisions for Above patterns"),
                    #          plotOutput("Comparision_Speed__AboveCharts"),
                    #          tags$h4("Total BP made Above"),
                    #          #verbatimTextOutput("PnL_Above"),
                    #          tableOutput("PnL_Above"),
                    #          tableOutput("PnL_ByBP_Above"),
                    #          tags$h4("Charts showing comparisions for Below patterns"),
                    #          plotOutput("Comparision_Speed__BelowCharts"),
                    #          tags$h4("Total BP made Below"),
                    #          tableOutput("PnL_Below"),
                    #          tableOutput("PnL_ByBP_Below"),
                    #          tags$h4(" All above Values."),
                    #          fixedRow(
                    #            column(12, dataTableOutput("ConfiTable_Above"))#tableOutput('table'))
                    #          ),
                    #          hr(),
                    #          tags$h4(" All below Values."),
                    #          fixedRow(
                    #            column(12, dataTableOutput("ConfiTable_Below"))#tableOutput('table'))
                    #          ),
                    #          value="panel2" ), 
                    # tabPanel("Conclusion",
                    #          #tags$h4("Profit made on this trade."),
                    #         # hr(),
                    #          fluidRow(
                    #            column(2, numericInput("StartFrom",'Start From',2, width = '100px')),
                    #            column(2, numericInput("EveryBP","Every", 1, width = '100px')),
                    #            column(2, numericInput("Offset","Offset", .5, width = '100px')),
                    #            column(2, numericInput("MaxSize","Max Size", 6, width = '100px')),
                    #            column(2, numericInput("DollarPerPoint","Dollar per Point", 100, width = '120px'))
                    #          ),
                    #          tags$h4("Final results in Profit Loss made."),
                    #         hr(),
                    #          fluidRow(
                    #            #column(12, div(tableOutput("BinsTable")))
                    #            column(12, div(dataTableOutput("TradeResults")))
                    #          ),
                    #          hr(),
                    #         tags$h4("Is this a good mean reversion strategy?"),
                    #         hr(),
                    #         fixedRow(
                    #           column(12, div( plotOutput("MeanReversionBellCurve")))
                    #         ),
                    #         hr(),
                    #         tags$h4("All Trade Data"),
                    #          fluidRow(
                    #            #column(12, div(tableOutput("BinsTable")))
                    #            column(12, div(dataTableOutput("TradeTable")))
                    #          ),
                    #          value="panel8" ), 
                    # tabPanel("Correlation",  
                    #          br(),
                    #          fluidRow(
                    #            column(12, plotOutput("CorrPlot"))
                    #          ),
                    #          hr(),
                    #          tags$h4("Volatility (1 SD move in Price):"),
                    #          fluidRow(
                    #            column(12, textOutput("VolatilityNumber"))
                    #          ),
                    #          hr(),
                    #          tags$h4("Half Life of Price:"),
                    #          fluidRow(
                    #            column(12,textOutput("HalfLife"))#tableOutput('table'))
                    #          ),
                    #          hr(),
                    #          tags$h4("Hust Exponent:"),
                    #          fluidRow(
                    #            column(12,htmlOutput("HustExpo"))#tableOutput('table'))
                    #          ),
                    #          value="panel5" ),
                    # tabPanel("Hedge Ratio",  
                    #            #tags$h4("Hedge Ratio:"),
                    #          fluidRow(
                    #            column(12,htmlOutput("HedgeRatioValue", inline=T))#tableOutput('table'))
                    #          ),
                    #          fluidRow(
                    #            column(12,plotOutput("HedgeRatio"))#tableOutput('table'))
                    #          ),
                    #          tags$h4("Regression Plot:"),
                    #          fluidRow(
                    #            column(12, plotOutput("RegressionPlot"))
                    #          ),
                    #          value="panel6" ),
                    # tabPanel("Cointegration",  
                    #          br(),
                    #          tags$h4("ADF Stationary Test:"),
                    #          fluidRow(
                    #            column(12, verbatimTextOutput("ADFTestStats"))
                    #          ),
                    #          hr(),
                    #          tags$h4("Johansen Cointegration Test:"),
                    #          fluidRow(
                    #            column(12, plotOutput("JohansonPlot"))
                    #          ),
                    #          fluidRow(
                    #            column(12, verbatimTextOutput("JohansonStats"))
                    #          ),
                    #          value="panel7" ),
                              hr()
        ),
        hr()
      )
    ),
    
    position = c("left"),
    fluid = T
  )#sidebarLayout
))


#runApp('D:/SHINYPROJECT',host="192.168.115.234",port=3700)

#runApp('D:/SHINYPROJECT',host="0.0.0.0",port=3700)
