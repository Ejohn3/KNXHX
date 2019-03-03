output$changeanother <- renderUI({
  actionButton('changeagain','Update Table Again?')
})



handsonChange <- reactive({
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
  test <- dbGetQuery(db,'SELECT * FROM wastedata')
  dbDisconnect(db)
  test
  
})



output$ChangeTable <- renderRHandsontable({
  if(is.null(handsonChange())){
    
    rhandsontable(data.frame())
  }else{
    DF <- handsonChange()
    DF$MONTH <- as.Date(DF$MONTH,'%m/%d/%Y')
    DF$AMOUNT_POUNDS <- as.numeric(DF$AMOUNT_POUNDS)
    rhandsontable(DF,height=800) %>%
      hot_col(col='MONTH') %>%
      hot_col('SOLID_WASTE_CATEGORY',readOnly=TRUE) %>%
      hot_col('LOCATION',readOnly=TRUE) %>%
      hot_col('TYPE',readOnly=TRUE) %>%
      hot_col('WASTE_TYPE',readOnly=TRUE) %>%
      hot_col('AMOUNT_POUNDS')
  }
  
})


observeEvent(input$changetbl, {
  DF <- hot_to_r(input$ChangeTable)
  DF$MONTH <- format(DF$MONTH,'%m/%d/%Y')
  DF <- DF %>% filter(SOLID_WASTE_CATEGORY %in% c("SWMF Recycled/Diverted (TONS)", "Commodity",
                                                  "Landfill", "Mulch (TONS)"))
  
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
  
  dbWriteTable(db,name='wastedata',value=DF,overwrite=TRUE,row.names=FALSE,header=TRUE)
  
  
  dbDisconnect(db)

})

observeEvent(input$changetbl,{
  
  shinyjs::hide("changetbl")
  shinyjs::hide("ChangeTable")
  shinyjs::show("save_msg2")
  shinyjs::show('changeanother')
  
})

observeEvent(input$changeanother,{
  shinyjs::show("changetbl")
  shinyjs::show("ChangeTable")
  shinyjs::hide("save_msg2")
  shinyjs::hide('changeanother')
  
  
})

