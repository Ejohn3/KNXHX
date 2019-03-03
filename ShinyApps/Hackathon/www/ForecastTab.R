source("/home/jlowhorn/ShinyApps/Hackathon/www/makeModels.R",  local = TRUE)



forecastdat <- reactive({
  
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
  
  tbl <-dbGetQuery(db,'SELECT * FROM knoxwaste.wastedata')
  
  dbDisconnect(db)
  tbl$MONTH <- as.Date(tbl$MONTH,'%m/%d/%Y')
  tbl
})

forecastagg <- reactive({
  tbl <- forecastdat()
  lastmon <- max(tbl$MONTH)
  agg <- tbl %>%
    filter(!is.na(AMOUNT_POUNDS)) %>%
    group_by(SOLID_WASTE_CATEGORY,LOCATION,TYPE,WASTE_TYPE) %>%
    summarize(count=n(),
              maxdate = max(MONTH)) %>%
    filter(count>=6) %>%
    filter(maxdate == lastmon) %>%
    select(-maxdate)
  agg
})



mademodels <- reactive({
  agg <- forecastagg()
  tbl <- forecastdat()
  
  models <- makeModels(agg,tbl)
  models
})

lenmodels <- reactive({
  len <- length(mademodels())
  vect <- c()
  for(i in 1:len){
    vect[i] <- paste0('Forecast',i)
  }
  as.character(vect)
})

output$texttest <- renderText(({
  
  lenmodels()
}))


output$forecastSelecter <- renderUI({
  selectInput('forecastpicker','Select Forecast',choices='')
})

observe(
  updateSelectInput(session,"forecastpicker",'Select Forecast',
                    choices=(lenmodels()))
)
