# feed arima prediction to xgb

# libraries
require(readr)
require(forecast)
require(xgboost)
#require(tsoutliers)

setwd("~/Kaggle/Rossman")
load("./Data/Rossman_merged_V3.RData")
test <- read.csv("./Data/test.csv")
store <- read.csv("./Data/store.csv")
store.loc <- read.csv("./Data/store_states.csv")


tstr <- md[md$split1 == 0,]
tstr <- tstr[tstr$DayOfWeek != 7,]
tsth <- md[md$split1 == -1,]
tsth <- tsth[tsth$DayOfWeek != 7,]
tste <- md[md$split1 == 2,]

sb <- sub.data("csb")

unique_stores <- unique(test$Store)

arima_xgb <- function(ii){
  
  st <- my.arima2(ii)
  model <- st[[1]]
  tsf <- st[[2]]
  resid <- model$residuals

  feature.names <- c("cYear", "cMonth", "cDayOfMonth", "cDayOfYear", "DayOfWeek", "Promo", "SchoolHoliday", "StateHoliday", "quarter", "cWeekOfYear")
  
  xtr <- tstr[tstr$Store == ii,]
  xth <- tsth[tsth$Store == ii,]
  xte <- tste[tste$Store == ii,]
  
  dt <- rbind(xtr, xth)
  tra <- dt[,feature.names]
  if(dt$Sales[1] == 0){
    
    tra <- tra[-1,]
    
  }
  

  outliers.index <- which(resid < (mean(resid) - 4*sd(resid)) | resid > (mean(resid) + 4*sd(resid)))
  tresid <- resid[-outliers.index]
  tra <- tra[-outliers.index,]
  
  set.seed(101)
  h <- sample(nrow(tra), 40)
  
  dtrain <- xgb.DMatrix(data.matrix(tra[-h,]), label = tresid[-h])
  dval <- xgb.DMatrix(data.matrix(tra[h,]), label = tresid[h])
  
  watchlist <- list(train = dtrain, val = dval)
  param <- list(objective = "reg:linear",
                max_depth = 8,
                eta = 0.005,
                subsample = 0.8,
                colsample_bytree = 0.7,
                min_child_weight = 5)
  
  
  bst <- xgb.train(params = param,
                   data = dtrain,
                   nround = 20000,
                   early.stop.round = 200,
                   print.every.n = 100,
                   maximize = FALSE,
                   feval = RMPSE,
                   watchlist = watchlist)
  
  b.ind <- bst$bestInd
  
  dtrain.f <- xgb.DMatrix(data.matrix(tra), label = tresid)
  watchlist2 <- list(train = dtrain.f)
  
  
  bst.f <- xgb.train(params = param,
                   data = dtrain,
                   nround = b.ind,
                   print.every.n = 100,
                   maximize = FALSE,
                   feval = RMPSE,
                   watchlist = watchlist2)
  
  pred <- predict(bst, data.matrix(xte[, feature.names]))
  
  final.forecast <- expm1(log(tsf+1) + pred)
  
  d <- sb[sb$Store == ii,]
  d <- cbind(d, arima = tsf)
  d <- cbind(d, arima_xgb = round(final.forecast, 3))


  fname = paste0("./ARIMA_XGB2/store_", ii, ".csv")
  write_csv(d, fname)
  cat("\n............................................")
  cat("\nForecasts obtained for store", ii)
  cat("\n............................................\n\n")
  
  
}

my.arima2 <- function(st, K = 4, check = FALSE) {
  
  cat("Obtaining forecast for store: ", st, "\n")
  
  xtr <- tstr[tstr$Store == st,]
  xth <- tsth[tsth$Store == st,]
  xte <- tste[tste$Store == st,]
  
  if(xtr[1,]$Sales == 0) {
    xtr <- xtr[-1,]
  }
  
  sun_te <- which(xte$DayOfWeek == 7)
  xte_ws <- xte[xte$DayOfWeek != 7,]
  
  len.ho <- nrow(xth)
  len.te <- nrow(xte_ws)
  
  if(xtr$CompetitionOpenSinceYear[1] < 21) {
    
        xreg_tr <- cbind(xtr$Open, xtr$Promo, xtr$SchoolHoliday, xtr$is_prev_holiday, xtr$is_next_holiday,
                         xtr$DayOfWeek2, xtr$DayOfWeek3, xtr$DayOfWeek4, xtr$DayOfWeek5,xtr$DayOfWeek6)
        
        xreg_th <- cbind(xth$Open, xth$Promo, xth$SchoolHoliday, xth$is_prev_holiday, xth$is_next_holiday, 
                         xth$DayOfWeek2, xth$DayOfWeek3, xth$DayOfWeek4, xth$DayOfWeek5,xth$DayOfWeek6)
        
        xreg_te <- cbind(xte_ws$Open, xte_ws$Promo, xte_ws$SchoolHoliday, xte_ws$is_prev_holiday, xte_ws$is_next_holiday, 
                         xte_ws$DayOfWeek2, xte_ws$DayOfWeek3, xte_ws$DayOfWeek4, xte_ws$DayOfWeek5,xte_ws$DayOfWeek6)
    
#     xreg_tr <- cbind(xtr$Open, xtr$Promo, xtr$SchoolHoliday, xtr$is_prev_holiday, xtr$is_next_holiday)
#     
#     xreg_th <- cbind(xth$Open, xth$Promo, xth$SchoolHoliday, xth$is_prev_holiday, xth$is_next_holiday)
#     
#     xreg_te <- cbind(xte_ws$Open, xte_ws$Promo, xte_ws$SchoolHoliday,  xte_ws$is_prev_holiday, xte_ws$is_next_holiday) 
    
    series <- ts(xtr$Sales, frequency = 6)
    m1 <- auto.arima(series, xreg = xreg_tr, seasonal = TRUE, approximation = FALSE)
    fc.m1 <- forecast(m1, xreg = xreg_th, h = len.ho)
    e1 <- rmspe.hout(expm1(fc.m1$mean), expm1(xth$Sales))
    # ev.matrix[ev.matrix$store_num == st,]$rmspe <- e1
    cat("RMSPE without fourier terms:", e1, "\n")
    
    z <- fourier(ts(xtr$Sales, frequency=313), K=4)
    zf <- fourierf(ts(xtr$Sales, frequency=313), K=4, h=len.ho)
    m2 <- auto.arima(series, xreg = cbind(z, xreg_tr), seasonal = TRUE, approximation = FALSE)
    fc.m2 <- forecast(m2, xreg = cbind(zf, xreg_th), h = len.ho)
    e2 <- rmspe.hout(expm1(fc.m2$mean), expm1(xth$Sales))
    # ev.matrix[ev.matrix$store_num == st,]$rmspe.fourier <- e2
    cat("RMSPE with fourier terms:", e2, "\n")
    
    xtr.whl <- rbind(xtr, xth)
    xreg.final <- cbind(xtr.whl$Open, xtr.whl$Promo, xtr.whl$SchoolHoliday, xtr.whl$is_prev_holiday, xtr.whl$is_next_holiday,
                        xtr.whl$DayOfWeek2, xtr.whl$DayOfWeek3, xtr.whl$DayOfWeek4, xtr.whl$DayOfWeek5,xtr.whl$DayOfWeek6)
    series <- ts(xtr.whl$Sales, frequency = 6)
    
    if(e1 < e2) {
      
      cat("\nmodel without fourier term chosen for store ", st)
      
      final.m <- auto.arima(series, 
                            xreg = xreg.final,
                            seasonal = TRUE,
                            approximation = FALSE)
      
      final.fc <- forecast(final.m,
                           xreg = xreg_te,
                           h = len.te)
      
    } else{
      
      cat("model with fourier term chosen for store ", st, "\n")
      z <- fourier(ts(xtr.whl$Sales, frequency=313), K=4)
      zf <- fourierf(ts(xtr.whl$Sales, frequency=313), K=4, h=len.te)    
      final.m <- auto.arima(series, 
                            xreg = cbind(z, xreg.final),
                            seasonal = TRUE,
                            approximation = FALSE)
      final.fc <- forecast(final.m,
                           xreg = cbind(zf, xreg_te),
                           h = len.te)
      
    }
  } else {
    
    cat("Competition entry found...")
    
        xreg_tr <- cbind(xtr$Open, xtr$Promo, xtr$SchoolHoliday, xtr$is_prev_holiday, xtr$is_next_holiday, xtr$isCompetition,
                         xtr$DayOfWeek2, xtr$DayOfWeek3, xtr$DayOfWeek4, xtr$DayOfWeek5,xtr$DayOfWeek6)
        
        xreg_th <- cbind(xth$Open, xth$Promo, xth$SchoolHoliday, xth$is_prev_holiday, xth$is_next_holiday, xth$isCompetition,
                         xth$DayOfWeek2, xth$DayOfWeek3, xth$DayOfWeek4, xth$DayOfWeek5,xth$DayOfWeek6)
        
        xreg_te <- cbind(xte_ws$Open, xte_ws$Promo, xte_ws$SchoolHoliday, xte_ws$is_prev_holiday, xte_ws$is_next_holiday, xte_ws$isCompetition,
                         xte_ws$DayOfWeek2, xte_ws$DayOfWeek3, xte_ws$DayOfWeek4, xte_ws$DayOfWeek5,xte_ws$DayOfWeek6) 
    
#     xreg_tr <- cbind(xtr$Open, xtr$Promo, xtr$SchoolHoliday, xtr$isCompetition, xtr$is_prev_holiday, xtr$is_next_holiday)
#     xreg_th <- cbind(xth$Open, xth$Promo, xth$SchoolHoliday, xth$isCompetition, xth$is_prev_holiday, xth$is_next_holiday)
#     xreg_te <- cbind(xte_ws$Open, xte_ws$Promo, xte_ws$SchoolHoliday, xte_ws$isCompetition, xte_ws$is_prev_holiday, xte_ws$is_next_holiday) 
    
    series <- ts(xtr$Sales, frequency = 6)
    m1 <- auto.arima(series, xreg = xreg_tr, seasonal = TRUE, approximation = FALSE)
    fc.m1 <- forecast(m1, xreg = xreg_th, h = len.ho)
    e1 <- rmspe.hout(expm1(fc.m1$mean), expm1(xth$Sales))
    # ev.matrix[ev.matrix$store_num == st,]$rmspe <- e1
    cat("RMSPE without fourier terms:", e1, "\n")
    
    z <- fourier(ts(xtr$Sales, frequency=313), K=4)
    zf <- fourierf(ts(xtr$Sales, frequency=313), K=4, h=len.ho)
    m2 <- auto.arima(series, xreg = cbind(z, xreg_tr), seasonal = TRUE, approximation = FALSE)
    fc.m2 <- forecast(m2, xreg = cbind(zf, xreg_th), h = len.ho)
    e2 <- rmspe.hout(expm1(fc.m2$mean), expm1(xth$Sales))
    # ev.matrix[ev.matrix$store_num == st,]$rmspe.fourier <- e2
    cat("RMSPE with fourier terms:", e2, "\n")
    
    xtr.whl <- rbind(xtr, xth)
    xreg.final <- cbind(xtr.whl$Open, xtr.whl$Promo, xtr.whl$SchoolHoliday, xtr.whl$is_prev_holiday, xtr.whl$is_next_holiday, xtr.whl$isCompetition,
                            xtr.whl$DayOfWeek2, xtr.whl$DayOfWeek3, xtr.whl$DayOfWeek4, xtr.whl$DayOfWeek5,xtr.whl$DayOfWeek6)
    
    # xreg.final <- cbind(xtr.whl$Open, xtr.whl$Promo, xtr.whl$SchoolHoliday, xtr.whl$is_prev_holiday, xtr.whl$is_next_holiday)
    series <- ts(xtr.whl$Sales, frequency = 6)
    
    if(e1 < e2) {
      
      cat("model without fourier term chosen for store ", st)
      
      final.m <- auto.arima(series, 
                            xreg = xreg.final,
                            seasonal = TRUE,
                            approximation = FALSE)
      
      final.fc <- forecast(final.m,
                           xreg = xreg_te,
                           h = len.te)
      
    } else{
      
      cat("model with fourier term chosen for store ", st, "\n")
      z <- fourier(ts(xtr.whl$Sales, frequency=313), K=4)
      zf <- fourierf(ts(xtr.whl$Sales, frequency=313), K=4, h=len.te)    
      final.m <- auto.arima(series, 
                            xreg = cbind(z, xreg.final),
                            seasonal = TRUE,
                            approximation = FALSE)
      final.fc <- forecast(final.m,
                           xreg = cbind(zf, xreg_te),
                           h = len.te)
      
    }
    
    
  }
  predictions <- c(expm1(final.fc$mean))
  
  for(i in 1:length(sun_te)) {
    
    predictions <- append(predictions, 0, after = sun_te[i]-1)
    
  }
#   
#   pred <- cbind.data.frame(Id = xte$Id, Store = xte$Store, Date = xte$Date, DoW = xte$DayOfWeek, Forecast = predictions)
#   fname <- paste0("./ARIMA/Run2/Forecast_Store_", st,".csv")
#   write_csv(pred, fname)
#   cat("\nSaved forecast in the file:", fname, "\n")
#   cat("\n.......................................................\n\n")
#   cat("Fetching next store...\n")
  rt <- list("model" = final.m, "forecasts" = predictions)
  return(rt)
}

# arima_xgb(3)
# arima_xgb(7)
# arima_xgb(8)
# arima_xgb(9)
# arima_xgb(10)
# arima_xgb(11)
# arima_xgb(12)
# # arima_xgb(13)
# arima_xgb(14)
# arima_xgb(15)
# arima_xgb(16)
# arima_xgb(19)
# arima_xgb(20)


for(st in unique_stores[50:150]){
  
  if (nrow(tstr[tstr$Store == st,]) > 700){
    
    arima_xgb(st)
  }else {
    
    cat("\n Store in BY...Fetching next store...")
  }
  
}
