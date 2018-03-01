library(ggplot2)
library(zoo)
library(dplyr)
library(xts)
library(forecast)
library(tseries)
library(KernSmooth)
library(RMySQL)

con <- dbConnect(MySQL(), user = "root", password = "", host= "localhost", dbname = "ab106", client.flag = CLIENT_MULTI_RESULTS)
sqlQuarter <- "select * from quarter_index"
quarterRows <- dbGetQuery(con, sqlQuarter)
dbDisconnect(con)

#changeOrder = function(dataName){
  c <- quarterRows$dataName
  a <- c[1:25]
  b <- c[26:225]
  d <- append(b, a)
  return(d)
}
#arimaTest = function(dataName){
  for(d in 1:2){
    for(p in 0:5){
      for(q in 0:5){
        result = arima(dataName, order = c(p, d, q), optim.control = list(maxit = 1000))
        print(result)
        print(p)
        print(d)
        print(q)
      }
    }
  }
}

str(quarterRows)
transactiontaipei <- quarterRows$"transaction%taipei"
transactiontaipei1 <- transactiontaipei[1:25]
transactiontaipei2 <- transactiontaipei[182:225]
transactiontaipei <- append(transactiontaipei2, transactiontaipei1)

economicGroth <- quarterRows$"economicGroth"
economicGroth1 <- economicGroth[1:25]
economicGroth2 <- economicGroth[26:225]
economicGroth <- append(economicGroth2, economicGroth1)
economicGroth <- economicGroth[economicGroth != 0]

GDPbyPerson <- quarterRows$"GDPbyPerson"
GDPbyPerson1 <- GDPbyPerson[1:25]
GDPbyPerson2 <- GDPbyPerson[26:225]
GDPbyPerson <-append(GDPbyPerson2, GDPbyPerson1)

cathayTaipeiIndex <- quarterRows$"cathayTaipeiIndex"
cathayTaipeiIndex1 <- cathayTaipeiIndex[1:25]
cathayTaipeiIndex2 <- cathayTaipeiIndex[26:225]
cathayTaipeiIndex <- append(cathayTaipeiIndex2, cathayTaipeiIndex1)
cathayTaipeiIndex <- cathayTaipeiIndex[cathayTaipeiIndex != 0]

sinyiTaipeiIndex <- quarterRows$"sinyiTaipeiIndex"
sinyiTaipeiIndex1 <- sinyiTaipeiIndex[1:25]
sinyiTaipeiIndex2 <- sinyiTaipeiIndex[26:225]
sinyiTaipeiIndex <- append(sinyiTaipeiIndex2, sinyiTaipeiIndex1)
sinyiTaipeiIndex <- sinyiTaipeiIndex[sinyiTaipeiIndex != 0]

tstransactiontaipei = zoo(transactiontaipei)
tseconomicGroth = zoo(economicGroth)
tsGDPbyPerson = zoo(GDPbyPerson)
tscathayTaipeiIndex = zoo(cathayTaipeiIndex)
tssinyiTaipeiIndex = zoo(sinyiTaipeiIndex)

Box.test(tstransactiontaipei)
Box.test(tseconomicGroth)
Box.test(tsGDPbyPerson)
Box.test(tscathayTaipeiIndex)
Box.test(tssinyiTaipeiIndex)

adf.test(tstransactiontaipei)
adf.test(tseconomicGroth)
adf.test(tsGDPbyPerson)
adf.test(tscathayTaipeiIndex)
adf.test(tssinyiTaipeiIndex)

ndiffs(tstransactiontaipei)
ndiffs(tseconomicGroth)
ndiffs(tsGDPbyPerson)
ndiffs(tscathayTaipeiIndex)
ndiffs(tssinyiTaipeiIndex)

transactiontaipei_arima = auto.arima(tstransactiontaipei)
economicGroth_arima = auto.arima(tseconomicGroth)
economicGroth_arima1 = arima(tseconomicGroth, order = c(4,1,4), fixed = c(0, 0, 0, NA, 0, 0, 0, NA))
#economicGroth_arima2 = arima(economicGroth, order = c(4,0,1))
GDPbyPerson_arima = auto.arima(tsGDPbyPerson)
#cathayTaipeiIndex_arima = auto.arima(tscathayTaipeiIndex)
macathayTaipeiIndex = ma(tscathayTaipeiIndex, 10)
sinyiTaipeiIndex_arima = auto.arima(tssinyiTaipeiIndex)

confint(transactiontaipei_arima)
confint(economicGroth_arima1)
confint(GDPbyPerson_arima)
#confint(cathayTaipeiIndex_arima)
confint(sinyiTaipeiIndex_arima)

forecast(transactiontaipei_arima, h = 4)
forecast(economicGroth_arima1, h = 4)
forecast(GDPbyPerson_arima, h = 4)
forecast(macathayTaipeiIndex, h = 4)
forecast(sinyiTaipeiIndex_arima, h = 4)

accuracy(forecast(transactiontaipei_arima, h = 4))
accuracy(forecast(economicGroth_arima1, h = 4))
accuracy(forecast(GDPbyPerson_arima, h = 4))
accuracy(forecast(macathayTaipeiIndex, h = 4))
accuracy(forecast(sinyiTaipeiIndex_arima, h = 4))

plot(forecast(transactiontaipei_arima, h = 4))
plot(forecast(economicGroth_arima1, h = 4))
plot(forecast(GDPbyPerson_arima, h = 4))
plot(forecast(macathayTaipeiIndex, h = 4))
plot(forecast(sinyiTaipeiIndex_arima, h = 4))

