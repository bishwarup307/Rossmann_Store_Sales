# load libraries
library(weatherData)
library(ggplot2)
library(scales)
library(plyr)

res<-list()
#Mannheim, HAmburg, K?lnBonn, M?nchen, Leipzig
todownload<-c("EDFM", "EDDH", "EDDK", "EDDM", "EDDP")
for (code in todownload){
  res[[code]]<-getWeatherForDate(code, "2013-01-01", "2015-10-01",
  # e.g. data not available for URSS getWeatherForDate(code, "2013-10-22", "2013-10-22",
                          station_type = "airportCode", opt_detailed = FALSE,
                           opt_write_to_file = FALSE, opt_temperature_columns = TRUE,
                           opt_all_columns = TRUE, opt_custom_columns = FALSE,
                           custom_columns = NULL, opt_verbose = FALSE, daily_min = FALSE,
                           daily_max = FALSE)
  if (is.null(res[[code]])) res[[code]] <- "ERROR"
  
  print("CURRENT LENGTH")
    print(length(res))
}
#detaileddata<-res
#LPFR#LPPR#URSSa

load("C:/Rossmann/weather.RData")
weatherdt<-rbind(res[[1]], res[[2]], res[[3]], res[[4]], res[[5]])
weatherdt$Date<-as.Date(weatherdt$Date)
setDT(weatherdt)
weatherdt[,list(mint=min(Min_TemperatureC), meant=mean(Mean_TemperatureC), maxt=max(Max_TemperatureC)), by=Date]
setDF(weatherdt)

aggregate(Mean_TemperatureC ~Date, res[[1]], mean)
weatherdt<-res[[1]]
weatherdt$Date<-as.Date(weatherdt$Date)
colnames(weatherdt)[1]<-"dt"
aggregate(Mean_TemperatureC ~ dt, weatherdt, mean)
for (i in 2:length(res)) weatherdt<-rbind(weatherdt, res[[i]])
setDT(weatherdt)


weatherdt<-weatherdt[,list(maxt=max(Max_TemperatureC), mint=min(Min_TemperatureC), meant=mean(Mean_TemperatureC)), by=Date]
weatherdt[,max(Max_TemperatureC), by=Date]
aggregate(cbind(x1, x2)~year+month, data=df1, sum, na.rm=TRUE)


## install from github if needed
# library(devtools)
# devtools::install_github("dvanclev/GTrendsR")

## load library
library(GTrendsR)
library(ISOweek)
ch <- gconnect('mardo@abicola.de', 'xxx')
res<-list()
terms<-c("rossmann", "grippe","nasenspray","rossmann angebote")
for (t in terms) res[[t]] <- gtrends(ch, t)

trendsdf<-data.frame(wk=unique(ISOweek(seq(as.Date("2013-01-01"), as.Date("2015-10-01"), "day"))))
for (t in terms) {
  currdf<-res[[t]]$trend
  currdf$wk<-ISOweek(currdf$start)
  trendsdf<-merge(trendsdf, currdf[,3:4], by="wk")
}
gtrends(ch, "\\%96ffnungszeiten Rossmannl")




for (code in names(res)){
  
  if (class(res[[code]])=="character") print(code)
  
}

accdf<-res[[1]]
accdf$id<-names(res)[1]

for (i in 2:length(res)){
  print(i)
  if (!class(res[[i]])=="character"){
    adddf<-res[[i]]
    code<-names(res)[i]
    adddf$id<-code
    accdf<-merge(accdf, adddf, all=TRUE)#accdf<-rbind(accdf, res[[i]])
  }
    
}

accdf1<-accdf[1:5000000,]
accdf2<-accdf[5000001:8000000,]
accdf3<-accdf[8000001:10090484,]

writeDataFrame(ch, "accdf1", "MARKUSD", "WEATHER",force=TRUE)
writeDataFrame(ch, "accdf2", "MARKUSD", "WEATHER")
writeDataFrame(ch, "accdf3", "MARKUSD", "WEATHER")

w2013 <- getWeatherForYear("sfo",2013)

w2013$shortdate <- strftime(w2013$Time, format="%y-%m-%d")

meanTemp <- ddply(w2013, .(shortdate), summarize, mean_T=mean(TemperatureF))
meanTemp$shortdate <- as.Date(meanTemp$shortdate,format="%y-%m-%d")

ggplot(meanTemp, aes(shortdate, mean_T)) + geom_line() +
  scale_x_date(labels=date_format("%m/%d")) + xlab("") + ylab("Mean Temp deg F") +
  ggtitle("2013 Average Daily Temperature at SFO")



d2014<-getWeatherForDate("EDFM", "2014-04-10", "2014-04-14",
                  station_type = "airportCode", opt_detailed = TRUE,
                  opt_write_to_file = FALSE, opt_temperature_columns = TRUE,
                  opt_all_columns = TRUE, opt_custom_columns = FALSE,
                  custom_columns = NULL, opt_verbose = FALSE, daily_min = FALSE,
                  daily_max = FALSE)