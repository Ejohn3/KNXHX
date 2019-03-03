##Initial Read of Global Data.. will be switched to DB pull


Waste <- reactive({
  
  options(mysql = list(
    "host" = "34.73.245.22",
    "port" = 3306,
    "user" = "shinyapp",
    "password" = "1234asdf1234"
  ))
  databaseName <- "knoxwaste"
  table <- "wastedata"
  
  db <- dbConnect(MySQL(), dbname = databaseName, host = options()$mysql$host, 
                  port = options()$mysql$port, user = options()$mysql$user, 
                  password = options()$mysql$password)
  
  test <- dbGetQuery(db,'SELECT * FROM test2')
  
  Waste <-dbGetQuery(db,'SELECT * FROM knoxwaste.wastedata')
  
  
  dbDisconnect(db)

  Waste
})

## Data Table for second tab/export and filter
DT <- reactive({
  
  
  DT <- Waste() %>%
    mutate(Date = as.Date(MONTH,format = '%m/%d/%Y')) %>%
    mutate(MonthYear = format(Date, format = '%B-%Y')) %>% 
    filter(MonthYear %in% input$MonthYear2) %>%
    filter(SOLID_WASTE_CATEGORY %in% input$SWC) %>%
    filter(LOCATION %in% input$Location) %>%
    filter(TYPE %in% input$Type) %>%
    filter(WASTE_TYPE %in% input$WasteType)%>%
    select(MonthYear, SOLID_WASTE_CATEGORY,LOCATION,TYPE,WASTE_TYPE)
  
  
})

## DT output for second tab

output$DT <- DT::renderDataTable({
  table <- datatable(DT(),extensions = c('FixedColumns','ColReorder','KeyTable','RowReorder','Scroller'),escape = FALSE,selection = 'single', rownames=FALSE,class = 'cell-border stripe',filter="top",
                     height = "200px",options = list(
                       searching=TRUE,
                       autoWidth=FALSE,
                       #keys=TRUE,
                       scroller=T,
                       scrollY=700,
                       paging=T,
                       dom = 'Bfrtip',
                       scrollX=T # end of buttons customization
                       
                       # customize the length menu
                       , list(c(5, 10, 15, 20, 25, -1), c('5', '10', '15', '20', '25', 'All'))# end of lengthMenu customization
                       , pageLength = 10
                       
                       
                       
                     ))
})


## Find total Commodity
Commodity <- reactive({
  
  
  
  DT <- Waste() %>%
    mutate(Date = as.Date(MONTH,format = '%m/%d/%Y')) %>%
    mutate(MonthYear = format(Date, format = '%B-%Y')) %>% 
    filter(MonthYear %in% input$MonthYear) %>%
    filter(WASTE_TYPE %in% c("Aluminum","Plastics (Commingled)","Glass","Mixed Paper","Cardboard","Steel/Tin","All Mixed (no glass)")) %>% 
    mutate(AMOUNT_POUNDS = as.numeric(AMOUNT_POUNDS)) %>% 
    summarise(Commodity = sum(AMOUNT_POUNDS)/2000)
  
  DT
  
  
})

## Find total Garbage

Garbage <- reactive({
  
  
  
  
  DT <- Waste() %>%
    mutate(Date = as.Date(MONTH,format = '%m/%d/%Y')) %>%
    mutate(MonthYear = format(Date, format = '%B-%Y')) %>% 
    filter(MonthYear %in% input$MonthYear) %>%
    filter(LOCATION == "Landfill Class 1") %>% 
    mutate(AMOUNT_POUNDS = as.numeric(AMOUNT_POUNDS)) %>% 
    summarise(Commodity = sum(AMOUNT_POUNDS))
  
  DT
  
  
  
})

##Find Total Mulch
Mulch <- reactive({
  
  
  
  
  DT <- Waste() %>%
    mutate(Date = as.Date(MONTH,format = '%m/%d/%Y')) %>%
    mutate(MonthYear = format(Date, format = '%B-%Y')) %>% 
    filter(MonthYear %in% input$MonthYear) %>%
    filter(SOLID_WASTE_CATEGORY == "Mulch (TONS)") %>% 
    mutate(AMOUNT_POUNDS = as.numeric(AMOUNT_POUNDS)) %>% 
    summarise(Commodity = sum(AMOUNT_POUNDS))
  
  DT
  
  
  
})

## Find Total C and D
CandD <- reactive({
  
  
  
  
  DT <- Waste() %>%
    mutate(Date = as.Date(MONTH,format = '%m/%d/%Y')) %>%
    mutate(MonthYear = format(Date, format = '%B-%Y')) %>% 
    filter(MonthYear %in% input$MonthYear) %>%
    filter(LOCATION == "Landfill Class 3") %>%
    mutate(AMOUNT_POUNDS = as.numeric(AMOUNT_POUNDS)) %>% 
    summarise(Commodity = sum(AMOUNT_POUNDS))
  
  DT
  
  
  
})

## Find total SWMF
SWMF <- reactive({
  
  
  
  
  DT <- Waste() %>%
    mutate(Date = as.Date(MONTH,format = '%m/%d/%Y')) %>%
    mutate(MonthYear = format(Date, format = '%B-%Y')) %>% 
    filter(MonthYear %in% input$MonthYear) %>%
    filter(TYPE == "Non-Commodity Recycling") %>%
    mutate(AMOUNT_POUNDS = as.numeric(AMOUNT_POUNDS)) %>% 
    summarise(Commodity = sum(AMOUNT_POUNDS))
  
  DT
  
  
  
})

## ValueBox for Commodity
output$CommodityRecycled <- shinydashboard::renderValueBox({
  shinydashboard::valueBox(
    paste0(Commodity()),tags$h2("COMMODITY RECYCLED"), icon = icon("recycle", lib = "font-awesome"),
    color = "red"
  )
})

## ValueBox For Garbage
output$Garbage <- shinydashboard::renderValueBox({
  shinydashboard::valueBox(
    paste0(Garbage()),tags$h2("Garbage (Class 1)"), icon = icon("trash", lib = "font-awesome"),
    color = "blue"
  )
})
##ValueBox For Mulch
output$Mulch <- shinydashboard::renderValueBox({
  shinydashboard::valueBox(
    paste0(Mulch()),tags$h2("MULCH RECYCLED"), icon = icon("leaf", lib = "font-awesome"),
    color = "green"
  )
})

##ValueBox for CandD
output$CandD <- shinydashboard::renderValueBox({
  shinydashboard::valueBox(
    paste0(CandD()),tags$h2("Garbage (Class 3)"), icon = icon("building", lib = "font-awesome"),
    color = "purple"
  )
})
##ValueBox for SWMF
output$SWMF <- shinydashboard::renderValueBox({
  shinydashboard::valueBox(
    paste0(SWMF()),tags$h2("SWMF"), icon = icon("truck", lib = "font-awesome"),
    color = "yellow"
  )
})
##
output$Header <- renderText({
  "Graphical Data Analysis"
})

output$MasterHeader <- renderText({
  "KPI OVERVIEW"
})

## Data for Barchart
Bar <- reactive({
  
  
  
  
  DT <- Waste() %>%
    mutate(Date = as.Date(MONTH,format = '%m/%d/%Y')) %>%
    mutate(MonthYear = format(Date, format = '%B-%Y')) %>% 
    
    mutate(Category = case_when(WASTE_TYPE == "Aluminum"| WASTE_TYPE =="Plastics (Commingled)"|WASTE_TYPE =="Glass"|WASTE_TYPE =="Mixed Paper"|WASTE_TYPE =="Cardboard"|WASTE_TYPE =="Steel/Tin"|WASTE_TYPE =="All Mixed (no glass)" ~ "Commodity",
                                SOLID_WASTE_CATEGORY == "Mulch (TONS)" ~ "Mulch",
                                TYPE == "Non-Commodity Recycling" ~"Recycled",
                                LOCATION == "Landfill Class 3"~"C&D (Class 3) Diverted",
                                LOCATION == "Landfill Class 1"~"Garbage (Class 1)")) %>%
    filter(!is.na(Category)) %>% 
    mutate(AMOUNT_POUNDS = as.numeric(AMOUNT_POUNDS)) %>%
    group_by(Category,Date) %>% 
    summarise(AMOUNT_POUNDS = sum(AMOUNT_POUNDS)) %>%
    mutate(AMOUNT_POUNDS = if_else(Category == "Commodity", AMOUNT_POUNDS/2000,AMOUNT_POUNDS)) %>% 
    
    select(Date,Category,AMOUNT_POUNDS)
  
  
  
  
})


## Render Pie Chart

output$pie <- renderHighchart({
  
  highchart() %>% 
    hc_chart(type = "pie") %>% 
    hc_add_series_labels_values(labels = Pie()$Category, values = Pie()$AMOUNT_POUNDS)
})


output$bar <- renderHighchart({
  
  hc <- hchart(Bar(), type = "column", hcaes(x = Date, y = AMOUNT_POUNDS, group = Category))
  
  hc
})
TS <- reactive({
  
  
  
  
  DT <- Waste() %>%
    mutate(Date = as.Date(MONTH,format = '%m/%d/%Y')) %>%
    mutate(MonthYear = format(Date, format = '%B-%Y')) %>% 
    
    mutate(Category = case_when(WASTE_TYPE == "Aluminum"| WASTE_TYPE =="Plastics (Commingled)"|WASTE_TYPE =="Glass"|WASTE_TYPE =="Mixed Paper"|WASTE_TYPE =="Cardboard"|WASTE_TYPE =="Steel/Tin"|WASTE_TYPE =="All Mixed (no glass)" ~ "Commodity",
                                SOLID_WASTE_CATEGORY == "Mulch (TONS)" ~ "Mulch",
                                TYPE == "Non-Commodity Recycling" ~"Recycled",
                                LOCATION == "Landfill Class 3"~"C&D (Class 3) Diverted",
                                LOCATION == "Landfill Class 1"~"Garbage (Class 1)")) %>%
    filter(!is.na(Category)) %>% 
    mutate(AMOUNT_POUNDS = as.numeric(AMOUNT_POUNDS)) %>%
    group_by(Category,Date) %>% 
    summarise(AMOUNT_POUNDS = sum(AMOUNT_POUNDS)) %>%
    mutate(AMOUNT_POUNDS = if_else(Category == "Commodity", AMOUNT_POUNDS/2000,AMOUNT_POUNDS)) %>% 
    
    select(Date,Category,AMOUNT_POUNDS)
  
  
  
  
})

output$ts <- renderHighchart({
  
  hc <- hchart(TS(), type = "line", hcaes(x = Date, y = AMOUNT_POUNDS, group = Category))
  
  hc
})

## Data for Pie Chart
Pie <- reactive({
  
  
  
  
  DT <- Waste() %>%
    mutate(Date = as.Date(MONTH,format = '%m/%d/%Y')) %>%
    mutate(MonthYear = format(Date, format = '%B-%Y')) %>% 
    filter(MonthYear %in% input$MonthYear) %>%
    
    mutate(AMOUNT_POUNDS = if_else(WASTE_TYPE %in% c("Aluminum","Plastics (Commingled)","Glass","Mixed Paper","Cardboard","Steel/Tin","All Mixed (no glass)"), AMOUNT_POUNDS/2000,AMOUNT_POUNDS)) %>% 
    mutate(Category = case_when(WASTE_TYPE == "Aluminum"| WASTE_TYPE =="Plastics (Commingled)"|WASTE_TYPE =="Glass"|WASTE_TYPE =="Mixed Paper"|WASTE_TYPE =="Cardboard"|WASTE_TYPE =="Steel/Tin"|WASTE_TYPE =="All Mixed (no glass)" | SOLID_WASTE_CATEGORY == "Mulch (TONS)" | TYPE == "Non-Commodity Recycling" ~"Recycled",
                                LOCATION == "Landfill Class 3"~"C&D (Class 3) Diverted",
                                LOCATION == "Landfill Class 1"~"Garbage (Class 1)")) %>%
    filter(!is.na(Category)) %>% 
    mutate(AMOUNT_POUNDS = as.numeric(AMOUNT_POUNDS)) %>%
    group_by(Category) %>% 
    summarise(AMOUNT_POUNDS = sum(AMOUNT_POUNDS)) %>%
    
    select(Category,AMOUNT_POUNDS)
  
  DT
  
  
})














## Data for Download Button
DOWNLOAD <- reactive({
  
  
  DT <- Waste() %>%
    mutate(Date = as.Date(MONTH,format = '%m/%d/%Y')) %>%
    mutate(MonthYear = format(Date, format = '%B-%Y')) %>% 
    filter(SOLID_WASTE_CATEGORY %in% input$SWC) %>%
    filter(LOCATION %in% input$Location) %>%
    filter(TYPE %in% input$Type) %>%
    filter(WASTE_TYPE %in% input$WasteType)%>%
    select(MonthYear, SOLID_WASTE_CATEGORY,LOCATION,TYPE,WASTE_TYPE)
  
  
  
  
  
})


## Download function
output$downloadData <- downloadHandler(
  filename = function() { paste('Export', '.csv', sep='') },
  content = function(file){
    write.csv(DOWNLOAD(), file)
  }
)
## vector for month year
MY <- reactive({
  
  
  DT <- Waste() %>%
    mutate(Date = as.Date(MONTH,format = '%m/%d/%Y')) %>%
    mutate(MonthYear = format(Date, format = '%B-%Y')) %>% 
    select(MonthYear)
  
  vec <- unique(DT)
})

## observe for Month Year
observe(
  updatePickerInput(session,
                    inputId = "MonthYear", 
                    label = "Date Selection", 
                    choices = MY(), 
                    selected = "January-2018"
  )
)
## second vector for month year for data table export dash
MY2 <- reactive({
  
  
  DT <- Waste() %>%
    mutate(Date = as.Date(MONTH,format = '%m/%d/%Y')) %>%
    mutate(MonthYear = format(Date, format = '%B-%Y')) %>% 
    select(MonthYear)
  
  vec <- unique(DT)
})
## observe event for second month year
observe(
  updatePickerInput(session,
                    inputId = "MonthYear2", 
                    label = "Date Selection", 
                    choices = MY2(), 
                    selected = "January-2018"
  )
)



## vector for solid waste category


vec <- reactive({
  vec <- unique(Waste()$SOLID_WASTE_CATEGORY)
})
## observe event for solid waste category
observe(
  updatePickerInput(session,
                    inputId = "SWC", 
                    label = "Solid Waste Category", 
                    choices = vec(), 
                    selected = vec()
  )
)




vecLocation <- reactive({
  vec <- unique(Waste()$LOCATION)
})
observe(
  updatePickerInput(session,
                    inputId = "Location", 
                    label = "Service Location", 
                    choices = vecLocation(), 
                    selected = vecLocation()
  )
)

vecType <- reactive({
  vec <- unique(Waste()$TYPE)
})
observe(
  updatePickerInput(session,
                    inputId = "Type", 
                    label = "Type", 
                    choices = vecType(), 
                    selected = vecType()
  )
)

vecWasteType <- reactive({
  vec <- unique(Waste()$WASTE_TYPE)
})
observe(
  updatePickerInput(session,
                    inputId = "WasteType", 
                    label = "Waste Type", 
                    choices = vecWasteType(), 
                    selected = vecWasteType()
  )
)




