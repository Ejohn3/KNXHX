output$submitanother <- renderUI({
  actionButton('submitanother','Submit another?')
})



handsonDT <- reactive({
	dat <- read_csv('/home/jlowhorn/ShinyApps/Hackathon/www/wasteinput.csv')
	names(dat) <- c('MONTH','SOLID_WASTE_CATEGORY','LOCATION','TYPE','WASTE_TYPE','AMOUNT_POUNDS')
	dat

})



output$PredTable <- renderRHandsontable({
	if(is.null(handsonDT())){
		
		rhandsontable(data.frame())
	}else{
	 DF <- handsonDT()
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


observeEvent(input$savetbl, {
  DF <- hot_to_r(input$PredTable)
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
  
  dbWriteTable(db,name='wastedata',value=DF,append=TRUE,row.names=FALSE,header=TRUE)
  
  
  dbDisconnect(db)
  
  
  
  
})

observeEvent(input$savetbl,{
  
  shinyjs::hide("PredTable")
  shinyjs::hide("savetbl")
  shinyjs::show("save_msg")
  shinyjs::show('submitanother')
  
  
  
})

observeEvent(input$submitanother,{
  shinyjs::show("PredTable")
  shinyjs::show("savetbl")
  shinyjs::hide("save_msg")
  shinyjs::hide('submitanother')

  
})







