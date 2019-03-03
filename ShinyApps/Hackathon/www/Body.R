body <- dashboardBody(inlineCSS("
                              #loading-content {
                                position: absolute;
                                background: #000000;
                                opacity: 0.9;
                                z-index: 100;
                                left: 0;
                                right: 0;
                                height: 100%;
                                text-align: center;
                                color: #FFFFFF;
                                }
                                "),
                      
                      # Loading message
                      div(
                        id = "loading-content",
                        h2("Loading...")
                      ),
                      
                      # The main app code goes here
                      hidden(
                        div(
                          id = "app-content",
                          p("Vital Analytics During Emergency Response")
                        )),
                      
                      tags$head(tags$link(rel = "stylesheet", type = "text/css", href = "custom.css")),
                      
                      
                      tabItems(tabItem(tabName = 'Overview',
                                       #       fluidRow(column(12,h1(textOutput("MasterHeader")),
                                       #                       tags$head(tags$style("#MeterA{color: black;
                                       # font-size: 30px;
                                       # font-style: bold;
                                       # }"
                                       #                       )
                                       #                       ))),
                                       # hr(),
                                       fluidRow(column(4,
                                                       pickerInput(
                                                         inputId = "MonthYear", 
                                                         label = ("Date Selection"),
                                                         choices = "", 
                                                         selected = "January-2018",
                                                         options = list(
                                                           `actions-box` = TRUE, 
                                                           size = 10,
                                                           `selected-text-format` = "count > 3"
                                                         ),multiple = FALSE)
                                                       
                                       ),
                                       column(4,shinydashboard::valueBoxOutput("CommodityRecycled",width=12)),
                                       column(4,shinydashboard::valueBoxOutput("Garbage",width=12))
                                       
                                       ),
                                       fluidRow(column(4,shinydashboard::valueBoxOutput("Mulch",width=12)),
                                                column(4,shinydashboard::valueBoxOutput("CandD",width=12)),
                                                column(4,shinydashboard::valueBoxOutput("SWMF",width=12))),
                                       fluidRow(column(12,h1(textOutput("Header")),
                                                       tags$head(tags$style("#MeterA{color: black;
                                                                            font-size: 30px;
                                                                            font-style: bold;
                                                                            }"
                                               )
                                                       ))),
                                       hr(),
                                       fluidRow(column(2,h2(class="radioSelect",radioButtons("col","Switch Plot",
                                                                                             choices = c("PIE", "BAR","TIME SERIES"),
                                                                                             selected = "BAR"))),
                                                column(10,
                                                       
                                                       conditionalPanel(
                                                         condition = "input.col == 'PIE'", highchartOutput("pie")),
                                                       conditionalPanel(
                                                         condition = "input.col == 'BAR'", highchartOutput("bar")),
                                                       conditionalPanel(
                                                         condition = "input.col == 'TIME SERIES'", highchartOutput("ts"))
                                                       
                                                       
                                                       
                                                       
                                                       
                                                ))
                                       
                                       
                                                       ),
                               
                               tabItem(tabName='Filter_Data',
                                       fluidRow(
                                         column(3,pickerInput(
                                           inputId = "MonthYear2", 
                                           label = ("Date Selection"),
                                           choices = "", 
                                           selected = "January-2018",
                                           options = list(
                                             `actions-box` = TRUE, 
                                             size = 10,
                                             `selected-text-format` = "count > 3"
                                           ),multiple = TRUE),
                                           
                                           
                                           pickerInput(
                                             inputId = "SWC", 
                                             label = ("Solid Waste Category"),
                                             choices = "", 
                                             selected = "",
                                             options = list(
                                               `actions-box` = TRUE, 
                                               size = 10,
                                               `selected-text-format` = "count > 3"
                                             ),multiple = TRUE),
                                           
                                           pickerInput(
                                             inputId = "Location", 
                                             label = ("Service Location"),
                                             choices = "", 
                                             selected = "",
                                             options = list(
                                               `actions-box` = TRUE, 
                                               size = 10,
                                               `selected-text-format` = "count > 3"
                                             ),multiple = TRUE),
                                           pickerInput(
                                             inputId = "Type", 
                                             label = ("Type"),
                                             choices = "", 
                                             selected = "",
                                             options = list(
                                               `actions-box` = TRUE, 
                                               size = 10,
                                               `selected-text-format` = "count > 3"
                                             ),multiple = TRUE),
                                           
                                           pickerInput(
                                             inputId = "WasteType", 
                                             label = ("Waste Type"),
                                             choices = "", 
                                             selected = "",
                                             options = list(
                                               `actions-box` = TRUE, 
                                               size = 10,
                                               `selected-text-format` = "count > 3"
                                             ),multiple = TRUE)
                                           
                                           
                                           
                                           
                                           
                                           
                                           
                                         ),
                                         column(9,downloadButton('downloadData', 'Download'),
                                                
                                                dataTableOutput('DT',height = "400px")))),
                      tabItem(tabName='HandsOnMenu',
                              fluidRow(actionButton(label='Append Table to Database',inputId='savetbl')),
                      				fluidRow(rHandsontableOutput('PredTable')),
                      				shinyjs::hidden(
                      				  uiOutput('submitanother'),
                      				  div(
                      				    id = "save_msg",
                      				    h3("Thanks, your data has been saved!")
                      				  ))

                      	                      ),
                      tabItem(tabName='ChangeData',
                              fluidRow(actionButton(label='Write Changes to Database',inputId='changetbl')),
                              fluidRow(rHandsontableOutput('ChangeTable')),
                              shinyjs::hidden(
                                uiOutput('changeanother'),
                                div(
                                  id = "save_msg2",
                                  h3("Thanks, your data has been saved!")
                                ))
                              
                              ),
                      # tabItem(tabName='forecasts',
                      #         fluidRow(
                      #           uiOutput('forecastSelecter'),
                      #           textOutput('texttest')
                      #         )
                      #         
                      #         ),
                      tabItem(tabName='Map',
                              leafletOutput("mymap",width = "100%" ,height = "940"))
                      
                      ))
