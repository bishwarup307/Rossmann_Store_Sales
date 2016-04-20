# load required libraries
require(zoo)
require(lubridate)
require(ProgGUIinR)
require(caret)

# set working directory & link to utils
setwd('F:/Kaggle/Rossman/Scripts')
source('final_utils.R')

# read train, test, store and location data files
cat("\nReading the data files ...\n")
train <- read.csv ("F:/Kaggle/Rossman/Data/train.csv")
test <- read.csv("F:/Kaggle/Rossman/Data/test.csv")
stores <- read.csv("F:/Kaggle/Rossman/Data/store.csv")
store.loc <- read.csv("F:/Kaggle/Rossman/Data/store_states.csv")
cat("Reading complete.\n")

# sales figures & date trafo
cat('\nformatting the date columns....\n')
train$Sales <- log(1+train$Sales)
train$Date <- as.Date(train$Date)
test$Date <- as.Date(test$Date)
cat('format complete.\n')

# derive store specific features
cat("\nMerge the store and store locations")
stores <- merge(stores, store.loc)
stores$State <- as.integer(as.factor(stores$State))

cat("\nPerforming store operations...\n")
stores$PromoInterval[stores$PromoInterval == ""] <- NA
stores$PromoInterval <- ifelse(stores$PromoInterval == "Jan,Apr,Jul,Oct", "Intv1",
                               ifelse(stores$PromoInterval == "Feb,May,Aug,Nov", "Intv2",
                                      ifelse(stores$PromoInterval == "Mar,Jun,Sept,Dec", "Intv3", NA)))

stores$coym <-  as.yearmon(paste(stores$CompetitionOpenSinceYear, stores$CompetitionOpenSinceMonth, sep = "-"))
stores$promo2SinceMonth <- ceiling(stores$Promo2SinceWeek/4)
stores$p2sym <- as.yearmon(paste(stores$Promo2SinceYear, stores$promo2SinceMonth, sep = "-"))

dmy <- dummyVars("~StoreType + Assortment + PromoInterval", data = stores)
trsf <- data.frame(predict(dmy, newdata = stores))
trsf[is.na(trsf)] <- 0
stores <- cbind(stores, trsf)
rm(trsf, dmy)
cat("store formatting complete.\n")

# merge train and test
cat("\nMerging store and sales...\n")
train <- merge(train, stores)
test <- merge(test, stores)

# order merged data by dates
cat("sorting sales by store number and date...\n")
train <- train[order(train$Store, train$Date),]
test <- test[order(test$Store, test$Date),]
train$Id <- -777
test.id <- test$Id
test$Sales <- (-1)
train <- train[, !names(train) %in% 'Customers']

cat("rearranging the columns...\n")
train <- train[,order(names(train))]
test <- test[,order(names(test))]

cat("Number of columns in train", ncol(train), "\n")
cat("Number of columns in test", ncol(test), "\n")
cat("column names same across train and test:", sum(names(train) == names(test)) == ncol(train))

cat("\nmerging test and train ...\n")
md <- rbind(train, test)
md$split1 <- 0
md$split1[md$Sales == -1] <- 2
md$split1[md$Date >= as.Date("2015-7-1") & md$Date < as.Date("2015-8-1")] <- -1
md$Open[is.na(md$Open)] <- 1
cat("Merge complete.\n")
gc()

# dummy code specific date features
cat("\nPerforming feature extractions...\n")
md$DayOfWeek <- as.character(md$DayOfWeek)
dmy <- dummyVars("~ DayOfWeek + StateHoliday", data = md)
trsf <- data.frame(predict(dmy, newdata = md))
md$DayOfWeek <- as.integer(md$DayOfWeek)
md <- cbind(md, trsf)
rm(trsf, dmy)

# extract basic temoral features
cat("extracting date features ...\n")
md$cYear <- year(md$Date)
md$cMonth <- month(md$Date)
md$cDayOfMonth <- as.integer(format(md$Date, "%d"))
md$cDayOfYear <- yday(md$Date) 
md$cWeekOfYear <- week(md$Date)
md$cWeekOfMonth <- week.of.month(md$cYear, md$cMonth, md$cDayOfMonth)
md$quarter <- quarter(md$Date)
md$yrwk <- as.integer(paste0(md$cYear, md$cWeekOfYear+10))
md$yrwk <- as.integer(as.factor(md$yrwk))
md$altWk <- ifelse(md$yrwk%%2 == 1, 1, 0)
md$MonthOfQuarter <- ifelse(md$cMonth%%3 == 1, 1,
                            ifelse(md$cMonth%%3 == 2, 2,
                                   ifelse(md$cMonth%%3 == 0, 3, 0)))

md$cYearMnth <-as.integer(as.factor(as.yearmon(md$Date)))
md$cMonthWeek <- as.integer(as.factor(paste(md$cMonth, md$cWeekOfMonth, sep = "-")))
md$cYearQuarter <- as.integer(as.factor(paste(md$cYear, md$quarter)))

operation.profile <-  md[, c('Store', 'Date')]
operation.profile <- operation.profile %>% group_by(Store) %>% dplyr::summarise(num.days = n()) 
ref.stores <- operation.profile[operation.profile$num.days == 806,]$Store
md$isRef <- ifelse(md$Store %in% ref.stores, 1, 0)

#Assumption of Mary adjustment
# md$is.AsM <- ifelse((md$Date == as.Date('2013-8-15') & md$StateHoliday.a == 1) | (md$Date == as.Date('2015-8-15') & md$StateHoliday.a == 1), 1, 0)
# Previous and next holidays indicator
# is.pASM <- c(0, md$is.AsM[-nrow(md)])
# md$is.pASM <- is.pASM
# md$is.nASM <- c(md$is.AsM[-1], 0)

# md$is.phol <- 0
# md$is.nhol <- 0

# cat("\nChecking for holidays...\n")
# for (ii in 2:(nrow(md)-1)) {
#
#   if(md$DayOfWeek[ii-1] != 7){
#
#     if(md$Date[ii-1] < md$Date[ii]) {                 # is.phol
#
#       if(md$Open[ii-1] == 0){
#
#         md$is.phol[ii] <- 1
#       }
#
#     }
#
#   }
#
#
# #     if(md$Open[ii-1] == 0 & md$Open[ii-2] == 0){
# #
# #       md$is.p2hol[ii] <- 1
# #
# #     }
#
#
#     if(md$DayOfWeek[ii+1] != 7 ) {
#
#       if(md$Date[ii+1] > md$Date[ii]){
#
#         if(md$Open[ii + 1] == 0){
#
#           md$is.nhol[ii] <- 1
#
#         }
#
#       }
#
#     }
#
# #     if(md$Open[ii+1] == 0 & md$Open[ii+2] == 0){
# #
# #       md$is.n2hol[ii] <- 1
# #     }
# #
#
#   print(paste("checking holidays for store:", md$Store[ii]))
# }

# cat("\nHolidays accounted for ...\n")


cat("\nExtracting a few more features ...\n")
md$isCompetition <- ifelse(as.yearmon(md$Date) < md$coym, 0, 1)                          # indicator for competition
md$isPromo2 <- ifelse(as.yearmon(md$Date) < md$p2sym, 0, 1)                              # indicator for promo2 scheme
md$isCompetition[is.na(md$isCompetition)] <- 0
md$isPromo2[is.na(md$isPromo2)] <- 0

cat("Deriving count features...\n")
md$stype_astype_cnt <- my.f2cnt(md, 'StoreType', 'Assortment')
md$state_stype_cnt <- my.f2cnt(md, 'State', 'StoreType')
md$state_astype_cnt <- my.f2cnt(md, 'State', 'Assortment')
md$state_stype_astype_cnt <- my.f3cnt(md, 'State', 'StoreType', 'Assortment')

cat("\nPreparing the final data...\n")
md$CompetitionOpenSinceYear <- as.integer(as.factor(md$CompetitionOpenSinceYear))
md$CompetitionOpenSinceYear[is.na(md$CompetitionOpenSinceYear)] <- 0
md$CompetitionOpenSinceMonth <- as.integer(as.factor(md$CompetitionOpenSinceMonth))
md$CompetitionOpenSinceMonth[is.na(md$CompetitionOpenSinceMonth)] <- 0
md$Promo2SinceYear <- as.integer(as.factor(md$Promo2SinceYear))
md$Promo2SinceYear[is.na(md$Promo2SinceYear)] <- 0
md$CompetitionDistance[is.na(md$CompetitionDistance)] <- median(stores$CompetitionDistance, na.rm = TRUE)
md$cYear <- as.integer(as.factor(md$cYear))
md$cMonth <- as.integer(as.factor(md$cMonth))

md$StoreType <- as.integer(as.factor(md$StoreType))
md$Assortment <- as.integer(as.factor(md$Assortment))
#md$StateHoliday <- as.integer(as.factor(md$StateHoliday))
md$PromoInterval <- as.integer(as.factor(md$PromoInterval))
md$PromoInterval[is.na(md$PromoInterval)] <- 0
gc()

cat("\nCreating the train data...\n")
ptr <- md[md$split1 == 0,]
ptr <- ptr[ptr$Open == 1,]
ptr <- ptr[ptr$Sales != 0,]
# ptr[1,]$is.phol <- 1
cat("Number of rows in training set:", nrow(ptr), "\n")
cat("Date range for training set", as.character(min(ptr$Date)), "to", as.character(max(ptr$Date)), "\n")


cat("\nCreating the test data...\n")
pte <- md[md$split1 == 2,]
cat("Number of rows in test set:", nrow(pte), "\n")
cat("Date range for test set", as.character(min(pte$Date)), "to", as.character(max(pte$Date)), "\n")

cat("\nCreating the hold-out set...\n")
pth <- md[md$split1 == -1,]
cat("Number of rows in test set:", nrow(pth), "\n")
cat("Date range for holdout set", as.character(min(pth$Date)), "to", as.character(max(pth$Date)), "\n")

cat("\n......... Preprocessing complete ...........\n")


################################
# time series data preparation #
################################
cat("\n....... Processing data for time series treatment ......\n")
tstr <- md[md$split1 == 0,]
tsth <- md[md$split1 == -1,]
tste <- md[md$split1 == 2,]

tstr <- tstr[tstr$DayOfWeek != 7,]
tstr[2,]$is.phol <- 1

tsth <- tsth[tsth$DayOfWeek != 7,]
store_num <- unique(tste$Store)

rmspe <- rep(-1, length(unique(tste$Store)))
rmspe.fourier <- rep(-1, length(unique(tste$Store)))
ev.matrix <- cbind.data.frame(store_num, rmspe, rmspe.fourier)

cat("state level aggregation and adjustments...")
cmb <- md[, c('Id', 'Store', 'StoreType', 'Assortment', 'State')]
dd <- cmb %>% group_by(State, StoreType, Assortment) %>% dplyr::summarise(n = n_distinct(Store))

#####################################################

md$coym <- NULL
md$p2sym <- NULL
md$state_holiday <- ifelse(as.character(md$StateHoliday) != '0' & md$Open == 0, 1, 0)

md <- md %>% group_by(Store) %>% mutate(is_prev_holiday = tlag(state_holiday, 1, Date))
md <- md %>% group_by(Store) %>% mutate(is_next_holiday = tlag(state_holiday, -1, Date))

md$is_prev_holiday[is.na(md$is_prev_holiday)] <- 0
md$is_next_holiday[is.na(md$is_next_holiday)] <- 0

md$t_unit <- as.integer(as.factor(as.integer(md$Date)))
md$t_unitsq <- md$t_unit^2