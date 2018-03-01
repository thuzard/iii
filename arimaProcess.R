#可能用到套件
install.packages('dplyr')
install.packages('zoo')
install.packages('ggplot2')
install.packages('xts')
install.packages('forecast')
install.packages("tseries")
install.packages('KernSmooth')
install.packages('quantmod')
install.packages('quantstrat')
library(ggplot2)
library(zoo)
library(dplyr)
library(xts)
library(forecast)
library(tseries)
library(KernSmooth)
library(quantmod)

#將資料讀進來後拆開
hindex = read.table("123.txt", sep = "|")
hindex1 = hindex[,1]  #年月
hindex2 = hindex[,2]  #台北房價指數
hindex3 = hindex[,3]  #新北房價指數
hindex4 = hindex[,4]  #大台北房價指數

#組合成時間序列
tshindex = zoo(hindex2)

#查看關於時間序列的資料
FinTS.stats(tshindex)

#檢查是否有自我相關
Box.test(tshindex)
AutocorTest(tshindex)

#Augmented Dickey-Fuller(ADf)檢定(可測試時間序列是否會回到長期平均值，也可檢測時間序列是否為stationary)，P值小於0.05或0.01，則拒絕虛無假設
adf.test(tshindex)
kpss.test(tshindex)

#一階差分求平穩化，最佳的差分可能為標準差最低的那一階，原資料acf圖的相關度越高，所需的差分階數也會更高
difftshindex = diff(tshindex)
#算出需要幾階差分才能達到平穩化
ndiffs(tshindex)

#取自然對數
log(tshindex)

#自我相關函數，決定MA階數
acf(difftshindex)

#偏自我相關函數，決定AR階數
pacf(difftshindex)

#平穩化處理後，若偏自相關函數是截尾的，而自相關函數是拖尾的，則建立AR模型
#若偏自相關函數是拖尾的，而自相關函數是截尾的，則建立MA模型
#若偏自相關函數和自相關函數均是拖尾的，則序列適合ARMA模型

#測試最佳的ARIMA模型
for(d in 1:2){
  for(p in 0:5){
    for(q in 0:5){
      result = arima(tshindex, order = c(p, d, q), optim.control = list(maxit = 1000))
      print(result)
      print(p)
      print(d)
      print(q)
    }
  }
}

#內建有自動偵測適合的ARIMA模型的方法
m = auto.arima(tshindex, seasonal = TRUE, stepwise = FALSE, trace = TRUE, approximation = FALSE, parallel = FALSE, allowdrift = TRUE)

#(3,2,4 479.42), (2,2,3 480.41), (2,1,5 480.94)

#此為ARIMA各係數的信賴區間，如果上下介裡包含0的話須考慮移除
confint(m)

etshindex = arima(tshindex, order = c(3,2,4), fixed = c(NA,NA,NA,NA,0,NA,NA)) #AIC = 477.49

#以ARIMA進行預測
predict(etshindex)
predict(etshindex, n.ahead = 10)  #預測未來十期
forecast(etshindex)  #和上面那句一樣功能，但會加上上下波動範圍
plot(forecast(etshindex,h=10))  #將forecast的結果畫出來
#預測準確度
accuracy(forecast(etshindex))   #只能放forcast物件

#https://people.duke.edu/~rnau/411home.htm 參考網站