require(readr)
require(xgboost)
setwd("F:/Kaggle/Rossman/")
set.seed(13)
load("./Data/ross.Rdata")
load("./Data/rossmann_final.RData")


fset1 <- md[, c("Store", "Date", "rossmann", "is_prev_holiday", "is_next_holiday", "pwin", "weekOfMonth",
                          "avgCustStoreDoW", "avgCustStore", "weekStart", "weekEnd", "Sales", "split1", "totalSchoolHolidays", "prev_Xmas",
                          "week_Xmas", "next_Xmas", "isCompetition", "week", "weekOfMonth", "promoRunningFor", "countryWideHolidayType",
                          "countryWideHolidayName", "stateWideHolidayType", "stateWideHolidayName", "PromoInterval", "")]

fset2 <- ross[, c("Store", "Date", "logdayofweeksales", "Promo", "logmonthlysales", "logdailysales",
                        "logyearlysales", "day", "DayOfWeek", "SpecialDay", "month", "satdayratio", "HolidayFactor", "CompetitionDistance",
                        "logreceipt", "year", "CompFactor", "State", "Store", "SchoolHoliday", "AnySchoolHoliday", "rain", "snow",
                        "Assortment", "sundayratio", "cluster", "CompOpenMonth", "MinTemp", "Promo2", "StateHoliday",
                        "MaxPrec", "Missing14", "refurbflag", "StoreType", "daysbeforerefurb", "MaxTemp", "dayssincerefurb", "Id",
                        "meddayofweeksales", "promofactor", "promo2aktiv")]

alldata <- merge(fset1, fset2, by = c("Store", "Date"))

ptr <- alldata[alldata$split1 == 0,]  # train data
pte <- alldata[alldata$split1 == 2,]  # test data

ptr <- ptr[ptr$Sales > 5.5 & ptr$Sales <= 10.5,]

feature.names <- names(alldata)[!names(alldata) %in% c("Id", "Sales", "Store.1", "Date", "split1")]

RMPSE<- function(preds, dtrain) {
  labels <- getinfo(dtrain, "label")
  elab<-exp(as.numeric(labels))-1
  epreds<-exp(as.numeric(preds))-1
  err <- round(sqrt(mean((epreds/elab-1)^2)), 6)
  return(list(metric = "RMPSE", value = err))
}

dtrain <- xgb.DMatrix(data = data.matrix(ptr[, feature.names]), label = ptr$Sales, missing = NA)

param = list(objective = "reg:linear",
             max_depth = 12,
             eta = 0.02,
             subsample = 0.7,
             colsample_bytree = 0.4,
             min_child_weight = 5,
             alpha = 1)

watchlist <- list(train = dtrain)

bst <- xgb.train(params = param,
                 data = dtrain,
                 nround = 3800,
                 print.every.n = 10,
                 watchlist = watchlist,
                 # feval = RMPSE,
                 maximize = FALSE)

pred <- predict(bst, data.matrix(pte[, feature.names]), missing = NA)
df <- data.frame(Id = pte$Id, Sales = expm1(pred))
df <- df[order(df$Id),]
write_csv(df, "./NaiveLearners/merged_features2.csv")


imp <- xgb.importance(feature.names, model = bst)
