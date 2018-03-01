install.packages('dplyr')
install.packages('zoo')
install.packages('ggplot2')
install.packages('xts')
install.packages('forecast')
install.packages("tseries")
install.packages('KernSmooth')
library(ggplot2)
library(zoo)
library(dplyr)
library(xts)
library(forecast)
library(tseries)
library(KernSmooth)
library(RMySQL)

con <- dbConnect(MySQL(), user = "root", password = "", host= "localhost", dbname = "ab106", client.flag = CLIENT_MULTI_RESULTS)
sqlMonth <- "select * from monthly_index"
monthlyRows <- dbGetQuery(con, sqlMonth)
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


str(monthlyRows)
leading_index <- monthlyRows$"leading_index"
leading_index = leading_index[leading_index != 0]

rent_index <- monthlyRows$"rent_index"
rent_index = rent_index[rent_index != 0]

rate <- monthlyRows$"rate"
rate = rate[rate != 0]

m1b <- monthlyRows$"m1b"
m1b = m1b[m1b != 0]

m2 <- monthlyRows$"m2"
m2 = m2[m2 != 0]

price_index <- monthlyRows$"price_index"
price_index = price_index[price_index != 0]

sinyi_taipei <- monthlyRows$"sinyi_taipei"
sinyi_taipei = sinyi_taipei[sinyi_taipei != 0]

tsleading_index = zoo(leading_index)
tsrent_index = zoo(rent_index)
tsrate = zoo(rate)
tsm1b = zoo(m1b)
tsm2 = zoo(m2)
tsprice_index = zoo(price_index)
tssinyi_taipei = zoo(sinyi_taipei)

Box.test(tsleading_index)
Box.test(tsrent_index)
Box.test(tsrate)
Box.test(tsm1b)
Box.test(tsm2)
Box.test(tsprice_index)
Box.test(tssinyi_taipei)

adf.test(tsleading_index)
adf.test(tsrent_index)
adf.test(tsrate)
adf.test(tsm1b)
adf.test(tsm2)
adf.test(tsprice_index)
adf.test(tssinyi_taipei)

ndiffs(tsleading_index)
ndiffs(tsrent_index)
ndiffs(tsrate)
ndiffs(tsm1b)
ndiffs(tsm2)
ndiffs(tsprice_index)
ndiffs(tssinyi_taipei)

leading_index_arima = auto.arima(tsleading_index)
rent_index_arima = auto.arima(rent_index)
rate_arima = auto.arima(rate)
m1b_arima = auto.arima(m1b)
m2_arima = auto.arima(m2)
price_index_arima = auto.arima(price_index)
sinyi_taipei_arima = auto.arima(sinyi_taipei)

confint(leading_index_arima)
confint(rent_index_arima)
confint(rate_arima)
confint(m1b_arima)
confint(m2_arima)
confint(price_index_arima)
confint(sinyi_taipei_arima)

forecast(leading_index_arima, h = 12)
forecast(rent_index_arima, h = 12)
forecast(rate_arima, h = 12)
forecast(m1b_arima, h = 12)
forecast(m2_arima, h = 12)
forecast(price_index_arima, h = 12)
forecast(sinyi_taipei_arima, h = 12)

accuracy(forecast(leading_index_arima))
accuracy(forecast(rent_index_arima))
accuracy(forecast(rate_arima))
accuracy(forecast(m1b_arima))
accuracy(forecast(m2_arima))
accuracy(forecast(price_index_arima))
accuracy(forecast(sinyi_taipei_arima))

plot(forecast(leading_index_arima, h = 12))
autoplot( forecast(leading_index_arima, h = 12))
plot(forecast(rent_index_arima, h = 12))
plot(forecast(rate_arima, h = 12))
plot(forecast(m1b_arima, h = 12))
plot(forecast(m2_arima, h = 12))
plot(forecast(price_index_arima, h = 12))
plot(forecast(sinyi_taipei_arima, h = 12))
