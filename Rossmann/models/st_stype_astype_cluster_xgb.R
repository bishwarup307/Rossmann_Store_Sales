# load libraries
require(readr)
require(dplyr)
require(forecast)
require(xgboost)

# set seed & wd
setwd("~/Kaggle/Rossman")
set.seed(2015)
#source("./Scripts/final_utils.R")

load("./Data/rossmann_merged_V5.RData")
train <- read.csv ("./Data/train.csv")
test <- read.csv("./Data/test.csv")
stores <- read.csv("./Data/store.csv")
store.loc <- read.csv("./Data/store_states.csv")

test_stores <- unique(test$Store)
clusters <- md %>% group_by(State, StoreType, Assortment) %>% summarise(n = n_distinct(Store))

for (ii in 1:nrow(clusters)) {
  
  cat("\n-------Fetching cluster",ii, "------------\n")
  cat("----------------------------------------------")
  cat("\nCluster details:")
  
  st <- clusters$State[ii]
  stype <- clusters$StoreType[ii]
  astype <- clusters$Assortment[ii]
  
  cat("\nState: ", st)
  cat("\nStoreType: ", stype)
  cat("\nAssortment: ", astype)
  
  dt <- find.groups(md, st, stype, astype)
  
  dt_tr <- dt[dt$split1 == 0,]
  dt_th <- dt[dt$split1 == -1,]
  dt_te <- dt[dt$split1 == 2,]
  
  if(nrow(dt_te) != 0){
  
    dt_tr <- dt_tr[dt_tr$Sales != 0,]
    dt_tr <- dt_tr[dt_tr$Open == 1,]
    
    dt_th <- dt_th[dt_th$Sales != 0,]
    dt_th <- dt_th[dt_th$Open == 1,]
    
    feature.names <- c("Store", "year", "month", "dayOfMonth", "DayOfWeek", "Promo", "pwin", "SchoolHoliday", "StateHoliday",
                       "CompetitionOpenSinceMonth", "CompetitionOpenSinceYear", "SundayOpen", "isRef", "is_prev_holiday", "is_next_holiday", "prev_Xmas",
                       "week_Xmas", "next_Xmas", "CompetitionDistance", "Promo2SinceWeek", "Promo2SinceYear", "weekOfYear", "SundayOpen")
    
    dtrain <- xgb.DMatrix(data = data.matrix(dt_tr[,feature.names]), labels = dt_tr$Sales)
    dval <- xgb.DMatrix(data = data.matrix(dt_th[,feature.names]), labels = dt_th$Sales)
    
    param = list(objective = "reg:linear",
                 max_depth = 10,
                 eta = 0.02,
                 subsample = 0.8,
                 colsample_bytree = 0.7,
                 min_child_weight = 6
    )
    watchlist = list(train = dtrain, val = dval)
    
    bst <- xgb.train(params = param,
                     data = dtrain,
                     nround = 3500,
                     print.every.n = 200,
                     watchlist = watchlist,
                     early.stop.round = 200,
                     feval = RMPSE,
                     maximize = FALSE)
    
    pred <- expm1(predict(bst, data.matrix(dt_te[,feature.names])))
    bScore <- rep(bst$bestScore, nrow(dt_te))
    sub <- data.frame(Id = dt_te$Id, Sales = pred, RMSPE = bScore)
    fname <- paste0("./Dropbox/PVT/ClusterXGB/Cluster_",st,"_", stype, "_",astype, ".csv")
    write_csv(sub, fname)
    cat("\n------ Cluster", ii, "evaluated-----\n")
  } else{
    
    cat("\n-------Cluster", ii, "not present in test-----\n")
    
  }
}
