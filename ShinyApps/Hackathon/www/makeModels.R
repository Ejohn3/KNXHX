
makeModels <- function(aggtable,rawdata){
  
  rq_LP  <-  function(x, Y, r=0.5, intercept=TRUE) {
    require("lpSolve")
    if (intercept) X  <-  cbind(1, x) else X <-  cbind(x)
    N   <-  length(Y)
    n  <-  nrow(X)
    stopifnot(n == N)
    p  <-  ncol(X)
    c  <-  c(rep(r, n), rep(1-r, n), rep(0, 2*p))  # cost coefficient vector
    A  <- cbind(diag(n), -diag(n), X, -X)
    res  <-  lp("min", c, A, "=", Y, compute.sens=1)
    sol <- res$solution
    coef1  <-  sol[(2*n+1):(2*n+2*p)]
    coef <- numeric(length=p)
    for (i in seq(along=coef)) {
      coef[i] <- (if(coef1[i]<=0)-1 else +1) *  max(coef1[i], coef1[i+p])
    }
    return(coef)
  }
  
  models <- list()
  for(i in 1:nrow(aggtable)){
    test <- rawdata %>%
      filter(SOLID_WASTE_CATEGORY==aggtable$SOLID_WASTE_CATEGORY[i]) %>%
      filter(LOCATION == aggtable$LOCATION[i] ) %>%
      filter(TYPE == aggtable$TYPE[i]) %>%
      filter(WASTE_TYPE == aggtable$WASTE_TYPE[i]) %>%
      mutate(AMOUNT_POUNDS = ifelse(is.na(AMOUNT_POUNDS),0,AMOUNT_POUNDS))
    
    if(nrow(test)<=6){
      
    }else{
    data <- test$AMOUNT_POUNDS
    
    
    Y.lag <- dplyr::lag(data)
    Y.lag <- Y.lag[2:(length(Y.lag))]
    Y <- Y.lag
    Y.hat <- data[2:(length(data))]
    mod.rq.coefs <- rq_LP(Y,Y.hat)
    
    mod.predict <- function(x){
      mod.rq.coefs[1] + mod.rq.coefs[2]*x
    }
    
    rqout <- mod.predict(data)
    
    Error <- abs(rqout-data)/data
    Error <- Error[is.finite(Error)]
    MAPE <- mean(Error,na.rm=T)
    Error <- (rqout-data)^2
    Error <- Error[is.finite(Error)]
    MSE <- mean(Error)
    
    forecasts <- 3
    
    p1 <- mod.rq.coefs[1] + mod.rq.coefs[2]*data[length(data)]
    p2 <- mod.rq.coefs[1] + mod.rq.coefs[2]*p1
    p3 <- mod.rq.coefs[1] + mod.rq.coefs[2]*p2
    
    startm <- max(test$MONTH,na.rm=T)
    
    crit_t <- qt(0.95, length(data)-1)
    
    pi_1 <- crit_t * sqrt(MSE * (1 + 1/(length(data)) + (p1-mean(data))^2/sum((data-mean(data))^2) ))
    pi_2 <- crit_t * sqrt(MSE * (1 + 1/(length(data)) + (p2-mean(data))^2/sum((data-mean(data))^2) ))
    pi_3 <- crit_t * sqrt(MSE * (1 + 1/(length(data)) + (p3-mean(data))^2/sum((data-mean(data))^2) ))
    
    if(p1 == 0 & p2 == 0 && p3 == 0){
      pi_1 <- 0
      pi_2 <- 0
      pi_3 <- 0
    }
    
    if(is.nan(MAPE)){
      MAPE<- 0
    }
    
    actuals <- test %>% mutate(FORECAST='Actual',
                               PREDICTIONINTERVAL=NA
                               )
    forecast <- data.frame(MONTH = c(startm %m+% months(1),startm %m+% months(2),startm %m+% months(3)),
                           SOLID_WASTE_CATEGORY = rep(unique(test$SOLID_WASTE_CATEGORY),3),
                           LOCATION=rep(unique(test$LOCATION),3),
                           TYPE=rep(unique(test$TYPE),3),
                           WASTE_TYPE=rep(unique(test$WASTE_TYPE),3),
                           AMOUNT_POUNDS = c(p1,p2,p3),
                           FORECAST='Forecast',
                           PREDICTIONINTERVAL = c(pi_1,pi_2,pi_3))
    forecast <- rbind(actuals,forecast)
    
    models[[i]]<-list(Foreacst=forecast,MAPE=MAPE)
    }
    
  }
  
  return(models)
  
}


