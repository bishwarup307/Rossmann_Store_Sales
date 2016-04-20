#########################
#seeds used:
# for indiv xgb: seed(1)
#########################

# load libraries
cat("loading required libraries...")
require(sqldf)
require(forecast)
require(xgboost)
require(reshape2)
require(dplyr)


#2 way count
my.f2cnt<-function(th2, vn1, vn2, filter=TRUE) {
  df<-data.frame(f1=th2[,vn1], f2=th2[,vn2], filter=filter)
  sum1<-sqldf("select f1, f2, count(*) as cnt from df where filter=1 group by 1,2")
  tmp<-sqldf("select b.cnt from df a left join sum1 b on a.f1=b.f1 and a.f2=b.f2")
  tmp$cnt[is.na(tmp$cnt)]<-0
  return(tmp$cnt)
}


#3 way count
my.f3cnt<-function(th2, vn1, vn2, vn3, filter=TRUE) {
  df<-data.frame(f1=th2[,vn1], f2=th2[,vn2], f3=th2[, vn3], filter=filter)
  sum1<-sqldf("select f1, f2, f3, count(*) as cnt from df where filter=1 group by 1,2, 3")
  tmp<-sqldf("select b.cnt from df a left join sum1 b on a.f1=b.f1 and a.f2=b.f2 and a.f3=b.f3")
  tmp$cnt[is.na(tmp$cnt)]<-0
  return(tmp$cnt)
}

# conditional mean for features
avg_sls <- function(df, st, att1, att2, y, filter, isOp, rdm = NULL) {
  
  d1 <- df[, c(st, att1, att2, y, isOp)]
  names(d1) <- c('st', 'f1', 'f2', 'a', 'isOp')
  d1$filter <- filter
  sum1 <- sqldf("select st, f1, f2, sum(isOp) as cnt, sum(a) as suma from d1 where filter = 1 group by 1, 2, 3")
  tmp <- sqldf("select b.suma, b.cnt from d1 a left join sum1 b on a.st = b.st and a.f1 = b.f1 and a.f2 = b.f2 ")
  tmp$cnt[is.na(sum1$cnt)] <- 0
  
  if(!is.null(rdm)) {
    
    tmp$avgs <- with(tmp, suma/cnt)
    tmp$avgs_adj <- with(tmp, (suma+1)/(cnt+1))          # credibility adjustment for the average
    set.seed(rdm)
    tmp$avgs_adj[filter] <- tmp$avgs_adj[filter]*(1+(runif(sum(filter), min = 0.3, max = 0.7)-0.5)*0.05)
    tmp$avgs_adj[is.na(tmp$avgs_adj)] <- 0
    return(tmp$avgs_adj)
  } else {
    
    tmp$avgs <- with(tmp, suma/cnt)
    tmp$avgs[is.na(tmp$avgs)] <- 0
    return(tmp$avgs)
  }
  
}

# check RMSPE on the hold-out set
rmspe.hout <- function(pred, act) {
  
  if(length(pred) != length(act)){
    
    print("actual and predicted array differ in size...")
    return(NA)
  } else {
    
    zt <- act != 0
    n <- sum(zt)
    nm <- act[zt] - pred[zt]
    dn <- act[zt]
    rmspe <- sqrt((1/n)*sum((nm/dn)^2))
    return(rmspe)
  }
  
}
# customized evaluation metric for xgboost
RMPSE<- function(preds, dtrain) {
    labels <- getinfo(dtrain, "label")
    elab<-exp(as.numeric(labels))-1
    epreds<-exp(as.numeric(preds))-1
    err <- sqrt(mean((epreds/elab-1)^2))
    return(list(metric = "RMPSE", value = err))
}

# customized evaluation metric for xgboost- v2
RMPSE_2 <- function(preds, dtrain) {
    labels <- getinfo(dtrain, "label")
    nonz <- labels != 0
    elab<-exp(as.numeric(labels[nonz]))-1
    epreds<-exp(as.numeric(preds[nonz]))-1
    err <- sqrt(mean((epreds/elab-1)^2))
    return(list(metric = "RMPSE2", value = err))
}

# conditional lag operator
tlag <- function(x, n = 1L, along_with){
    index <- match(along_with - n, along_with, incomparable = NA)
    out <- x[index]
    attributes(out) <- attributes(x)
    out
}

# previous holiday indicator
# prev.holdiday <- function(th) {
#   
#   phol <- c()
#   unique.stores <- unique(th$Store)
#   for (i in 1:length(unique.stores)) {
#     
#     dt <- th[th$Store == unique.stores[i],]
#     holidays <- th[th$s]
#     
#   }
#   
#   
# }


# fourier adjustment for the store sales
fourier.trans <- function(sb, K = 4) {
  
  sb.dates <- sb$Date
  a <- seq(as.Date("2013-1-1"), as.Date("2015-9-17"), by = "day")
  time.df <- cbind.data.frame(Date = a)
  whl.wdw <- merge(time.df, sb, all.x = TRUE)
  whl.wdw$adj_Sales[is.na(whl.wdw$adj_Sales)] <- 0              # modified for state-level xgb(adj_Sales)
  whl.wdw$split1[is.na(whl.wdw$split1)] <- -333
  whl.wdw$flag <- ifelse(whl.wdw$split1 == 0 | whl.wdw$split1 == -1 |whl.wdw$split1 == -333, 1, 0)
  whl.wdw <- whl.wdw[order(whl.wdw$Date),]
  
  pt <- whl.wdw[whl.wdw$flag == 1,]$adj_Sales                   # modified for state-level xgb(adj_Sales)
  horizon <- nrow(sb[sb$split1 == 2,])
  
  z1 <- data.frame(fourier(ts(pt, frequency = 14), K = 4))
  zf1 <- data.frame(fourierf(ts(pt, frequency = 14), K = 4, h = horizon))
  z2 <- data.frame(fourier(ts(pt, frequency = 365), K = 4))
  zf2 <- data.frame(fourierf(ts(pt, frequency = 365), K = 4, h = horizon))
  z3 <- data.frame(fourier(ts(pt, frequency = 182.5), K = 4))
  zf3 <- data.frame(fourierf(ts(pt, frequency = 182.5), K = 4, h = horizon))
  
  k1 <- rbind(z1, zf1)
  k2 <- rbind(z2, zf2)
  k3 <- rbind(z3, zf3)
  nm <- c(names(k1),names(k2), names(k3))
  k <- cbind(k1, k2)
  k <- cbind(k, k3)
  whl.wdw <- cbind(whl.wdw, k)
  
  whl.wdw <- whl.wdw[whl.wdw$Date %in% sb.dates,]
  ff <- whl.wdw[, names(whl.wdw) %in% nm]
  return (ff)
} 


get.features <- function(th, st) {
  
# feature set for indiv_xgb first run
#   feature.names <- c('Id', 'Sales', 'Promo', 'DayOfWeek1', 'DayOfWeek2', 'DayOfWeek3', 'DayOfWeek4', 'DayOfWeek5', 'DayOfWeek6', 'DayOfWeek7',
#                     'cYear', 'cMonth', 'cDayOfMonth', 'cDayOfYear', 'cWeekOfYear', 'cWeekOfMonth','quarter', 'DayOfWeek', 'MonthOfQuarter',
#                     'isCompetition', 'SchoolHoliday', 'StateHoliday', 'split1', 'Date', 'altWk', 'is.phol', 'is.nhol')
  
# feature set for state level xgb first run
  feature.names <- c( 'Id', 'Store',
                      'adj_Sales', 'Promo', 'DayOfWeek1', 'DayOfWeek2', 'DayOfWeek3', 'DayOfWeek4', 'DayOfWeek5', 'DayOfWeek6', 'DayOfWeek7',
                     'cYear', 'cMonth', 'cDayOfMonth', 'cDayOfYear', 'cWeekOfYear', 'cWeekOfMonth','quarter', 'MonthOfQuarter',
                     'isCompetition', 'SchoolHoliday', 'StateHoliday', 'split1', 'Date', 'altWk', 'is.phol', 'is.nhol', 'DayOfWeek')
  
  model.data <- th[th$Store == st,]
  model.data <- model.data[,feature.names]
  
  ff <- fourier.trans(model.data)
  
  model.data <- cbind(model.data, ff)
  return(model.data)
  
}

# execute xgb model in store level
indiv.xgb <- function(th, st) {
  
  dt <- get.features(th, st)
  tra <- dt[dt$split1 == 0,]
  hold <-  dt[dt$split1 == -1,]
  te <- dt[dt$split1 == 2,]
  
  tra$adj_Sales[tra$adj_Sales < 1] <- 0
  hold$adj_Sales[hold$adj_Sales < 1] <- 0
  tra <- tra[tra$adj_Sales != 0,]                                                     # adjusted for state level xgb
  hold <- hold[hold$adj_Sales != 0,]                                                  # adjusted for state level xgb
  
  features <- names(dt)[!names(dt) %in% c('Id','adj_Sales', 'Date', 'split1', 'flag', 'DayOfWeek')]
  
  dtrain <- xgb.DMatrix(data=data.matrix(tra[, features]),label=tra$adj_Sales)          # adjusted for state level xgb
  dval <- xgb.DMatrix(data=data.matrix(hold[, features]),label=hold$adj_Sales)          # adjusted for state level xgb
  watchlist<-list(val=dval,train=dtrain)
  
  param <- list(  objective           = "reg:linear", 
                  booster             = "gbtree",
                  eta                 = 0.005,
                  max_depth           = 8, 
                  subsample           = 0.7, 
                  colsample_bytree    = 0.6,
                  num_parallel_tree   = 6
                  
  )
  set.seed(3)
  clf <- xgb.train(   params              = param, 
                      data                = dtrain, 
                      nrounds             = 3000,
                      verbose             = 1,
                      early.stop.round    = 120,
                      watchlist           = watchlist,
                      maximize            = FALSE,
                      feval               = RMPSE
  )
  
  b.Score <- rep(clf$bestScore, nrow(te))
  pred <- expm1(predict(clf, data.matrix(te[,features])))
  sub <- data.frame(Id = te$Id, Date = te$Date, DoW = te$DayOfWeek, Forecast = pred, best_RMSPE = b.Score)
  fname = paste0("F:/Kaggle/Rossman/StateLevelXgb/svdF_", st,".csv")
  write.csv(sub, file = fname, row.names = FALSE)
  cat("\nForecasts written in file", fname)
  gc()
}

# execute state level xgb
state.xgb <- function(th) {
  
  num.comb <- nrow(dd)
  for (i in 1:80) {
    
    cllc <- find.groups(th, state = dd$State[i], st.type = dd$StoreType[i], ast.type = dd$Assortment[i])
    cllc <- noise_rd(cllc)
    filt.stores <- unique(cllc$Store)
    test.stores <- unique(test$Store)
    for (s in 1:length(filt.stores)) {
      
      if(filt.stores[s] %in% test.stores) {
        
        indiv.xgb(cllc, filt.stores[s])
        
      }else {
        
        cat("\nStore", filt.stores[s], "SKIPPED\n")
        
      }
      
    }
    
  }
    
}


# state-storetype-assortment level clusters
find.groups <- function(th, state, st.type, ast.type){
  
  store.collection <- th[ which(th$State == state & th$StoreType == st.type & th$Assortment == ast.type),]
  return(store.collection)
  
}

#svd for smoothing the sales line in state-storetype-assortment level
noise_rd <- function(df) {
  
  if(length(unique(df$Store)) < 5) {
    
    df$adj_Sales <- df$Sales
    return(df)
  } else {
    
      df <- df[order(df$Store, df$Date),]
      sub.ids <- df$Id
      
      df.part <- df[c('Store', 'Date', 'Sales')]
      agg <- dcast(df.part, Date ~ Store)
      agg[is.na(agg)] <- 0
      num.store <- ncol(agg) - 1
      ncomp = ceiling(0.35*num.store)
      z <- svd(agg[, 2:ncol(agg)], nu = ncomp, nv = ncomp)
      s <- diag(z$d[1:ncomp])
      agg[, 2:ncol(agg)] <- z$u %*% s %*% t(z$v)
      agg <- melt(agg, id = 'Date')
      names(agg) <- c('adj_Date', 'adj_Store', 'adj_Sales')
      
      #agg <- agg[!(agg$adj_Store == '988' & agg$adj_Date == as.Date('2013-1-1')),]    # store 988 has missing 2013-1-1
      df <- cbind(df, agg)
      return(df)
  } 

}

# this function evaluates if a fourier term is required or not and
# then fits the ARIMA model in store level
my.arima <- function(st, K = 4, check = FALSE) {
  
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
  # evaluate whether to include competition factor in the model
  if(xtr$CompetitionOpenSinceYear[1] < 21) {
    
    xreg_tr <- cbind(xtr$Open, xtr$Promo, xtr$SchoolHoliday, xtr$is.phol, xtr$is.nhol,
                     xtr$DayOfWeek2, xtr$DayOfWeek3, xtr$DayOfWeek4, xtr$DayOfWeek5,xtr$DayOfWeek6)
    
    xreg_th <- cbind(xth$Open, xth$Promo, xth$SchoolHoliday, xth$is.phol, xth$is.nhol, 
                     xth$DayOfWeek2, xth$DayOfWeek3, xth$DayOfWeek4, xth$DayOfWeek5,xth$DayOfWeek6)
    
    xreg_te <- cbind(xte_ws$Open, xte_ws$Promo, xte_ws$SchoolHoliday, xte_ws$is.phol, xte_ws$is.nhol, 
                     xte_ws$DayOfWeek2, xte_ws$DayOfWeek3, xte_ws$DayOfWeek4, xte_ws$DayOfWeek5,xte_ws$DayOfWeek6) 
    
    series <- ts(xtr$Sales, frequency = 12)
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
    xreg.final <- cbind(xtr.whl$Open, xtr.whl$Promo, xtr.whl$SchoolHoliday, xtr.whl$is.phol, xtr.whl$is.nhol, 
                        xtr.whl$DayOfWeek2, xtr.whl$DayOfWeek3, xtr.whl$DayOfWeek4, xtr.whl$DayOfWeek5,xtr.whl$DayOfWeek6)
    series <- ts(xtr.whl$Sales, frequency = 12)
    
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
  } else {
    
    cat("Competition entry found...")
    
    xreg_tr <- cbind(xtr$Open, xtr$Promo, xtr$SchoolHoliday, xtr$is.phol, xtr$is.nhol, xtr$isCompetition,
                     xtr$DayOfWeek2, xtr$DayOfWeek3, xtr$DayOfWeek4, xtr$DayOfWeek5,xtr$DayOfWeek6)
    
    xreg_th <- cbind(xth$Open, xth$Promo, xth$SchoolHoliday, xth$is.phol, xth$is.nhol, xth$isCompetition,
                     xth$DayOfWeek2, xth$DayOfWeek3, xth$DayOfWeek4, xth$DayOfWeek5,xth$DayOfWeek6)
    
    xreg_te <- cbind(xte_ws$Open, xte_ws$Promo, xte_ws$SchoolHoliday, xte_ws$is.phol, xte_ws$is.nhol, xte_ws$isCompetition,
                     xte_ws$DayOfWeek2, xte_ws$DayOfWeek3, xte_ws$DayOfWeek4, xte_ws$DayOfWeek5,xte_ws$DayOfWeek6) 
    
    series <- ts(xtr$Sales, frequency = 12)
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
    xreg.final <- cbind(xtr.whl$Open, xtr.whl$Promo, xtr.whl$SchoolHoliday, xtr.whl$is.phol, xtr.whl$is.nhol, xtr.whl$isCompetition,
                        xtr.whl$DayOfWeek2, xtr.whl$DayOfWeek3, xtr.whl$DayOfWeek4, xtr.whl$DayOfWeek5,xtr.whl$DayOfWeek6)
    series <- ts(xtr.whl$Sales, frequency = 12)
    
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
  
  pred <- cbind.data.frame(Id = xte$Id, Store = xte$Store, Date = xte$Date, DoW = xte$DayOfWeek, Forecast = predictions)
  fname <- paste0("Forecast_Store_", st,".RData")
  save(pred, file = fname)
  cat("Saved forecast in the file:", fname, "\n")
  cat("\n.......................................................\n\n")
  cat("Fetching next store...\n")
}

# Assumption of Mary (15th Aug) adjustment for Bavaria
assmp.adj <- function(th, te) {
  
  assmp.state <- 'BY'

  ss <- subset(te, State == assmp.state)
  ss$adj_fc <- ss$Sales
  all.stores <- unique(ss$Store)
  
  for (i in all.stores) {
    
    s1 <- th$Sales[which(th$Store == i & th$Date == as.Date('2013-08-02'))]
    s2 <- th$Sales[which(th$Store == i & th$Date == as.Date('2013-08-16'))]
    adj_factor <- (expm1(s2)+2100)/(expm1(s1)+2100)
    s <- ss$Sales[which(ss$Store == i & ss$Date == as.Date('2015-08-03'))]
    ss$adj_fc[which(ss$Store == i & ss$Date == as.Date('2015-08-17'))] <- s*adj_factor
    cat('Assumption of Mary effect on Store: ', i, "\n")
  }
  
  a <- ss[, c('Id', 'adj_fc')]
  b <- merge(te, a, all.x = TRUE)
  b$adj_fc[is.na(b$adj_fc)] <- b$Sales[is.na(b$adj_fc)]
  b$cmp <- ifelse(b$Sales == b$adj_fc, 0, 1)
  b <- b[order(b$Store, b$Date),]
  return(b)
}

# read a submission and merge with test and store
sub.data <- function(file) {
  
  fileName <- paste0("F:/Kaggle/Rossman/Submissions/", file, ".csv")
  sub <- read.csv(fileName)
  
  if(class(test$Date) != 'Date') {
    
    test$Date <- as.Date(test$Date)
    
  }
  
  k <- merge(test, sub)
  k <- merge(k, store.loc)
  k <- k[, c('Id', 'State', 'Store', 'Date', 'DayOfWeek', 'Open', 'Promo', 'Sales')]
  k <- k[order(k$Store, k$Date),]
  return(k)
}